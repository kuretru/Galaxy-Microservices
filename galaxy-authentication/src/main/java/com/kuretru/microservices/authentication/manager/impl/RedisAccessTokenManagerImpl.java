package com.kuretru.microservices.authentication.manager.impl;

import com.kuretru.microservices.authentication.entity.AccessTokenBO;
import com.kuretru.microservices.authentication.entity.AccessTokenDO;
import com.kuretru.microservices.authentication.entity.AccessTokenDTO;
import com.kuretru.microservices.authentication.manager.AccessTokenManager;
import com.kuretru.microservices.authentication.property.AuthenticationProperty;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.exception.ServiceException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.util.Set;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Slf4j
@Service
@ConditionalOnBean(StringRedisTemplate.class)
public class RedisAccessTokenManagerImpl implements AccessTokenManager {

    private static final String REDIS_ROOT_KEY = "AccessTokenManager.";
    private static final String REDIS_ACCESS_TOKEN_KEY = REDIS_ROOT_KEY + "accessToken.";

    private final AuthenticationProperty property;
    private final RedisTemplate<String, Serializable> redisTemplate;

    @Autowired
    public RedisAccessTokenManagerImpl(AuthenticationProperty property, RedisTemplate<String, Serializable> redisTemplate) {
        this.property = property;
        this.redisTemplate = redisTemplate;
    }

    @Override
    public AccessTokenDTO generate(UUID userId, Set<String> roles) {
        // 7BF29055-[1C77-4287-8FB2]-D2FB73480856
        String id = UUID.randomUUID().toString().substring(9, 23);
        while (Boolean.TRUE.equals(redisTemplate.hasKey(buildKey(id)))) {
            id = UUID.randomUUID().toString().substring(9, 23);
        }

        AccessTokenDO value = new AccessTokenDO(
                UUID.randomUUID().toString().replace("-", ""),
                userId,
                roles
        );
        redisTemplate.opsForValue().set(buildKey(id), value, property.getExpireTime());
        if (log.isInfoEnabled()) {
            log.info("已保存用户AccessToken凭据");
        }

        return new AccessTokenDTO(id, value.getSecret());
    }

    @Override
    public AccessTokenBO get(String id) throws ServiceException {
        String key = buildKey(id);
        exist(key);
        AccessTokenDO value = (AccessTokenDO)redisTemplate.opsForValue().getAndExpire(key, property.getExpireTime());
        assert value != null;
        return new AccessTokenBO(value.getSecret(), value.getUserId(), value.getRoles());
    }

    @Override
    public void refresh(String id) throws ServiceException {
        String key = buildKey(id);
        exist(key);
        redisTemplate.expire(key, property.getExpireTime());
    }

    @Override
    public void revoke(String id) throws ServiceException {
        String key = buildKey(id);
        exist(key);
        redisTemplate.delete(key);
    }

    private void exist(String key) throws ServiceException {
        if (Boolean.FALSE.equals(redisTemplate.hasKey(key))) {
            throw ServiceException.build(UserErrorCodes.ACCESS_PERMISSION_ERROR, "登录已过期，请重新登录");
        }
    }

    private String buildKey(String key) {
        return REDIS_ACCESS_TOKEN_KEY + key;
    }

}
