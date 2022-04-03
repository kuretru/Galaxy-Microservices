package com.kuretru.microservices.oauth2.server.memory.impl;

import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.oauth2.common.constant.OAuth2Constants;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AccessTokenDTO;
import com.kuretru.microservices.oauth2.common.entity.OAuth2ErrorEnum;
import com.kuretru.microservices.oauth2.common.entity.OAuth2Triple;
import com.kuretru.microservices.oauth2.common.exception.OAuth2Exception;
import com.kuretru.microservices.oauth2.server.entity.OAuth2AccessTokenBO;
import com.kuretru.microservices.oauth2.server.entity.OAuth2AccessTokenDO;
import com.kuretru.microservices.oauth2.server.memory.OAuth2AccessTokenMemory;
import com.kuretru.microservices.oauth2.server.property.OAuth2ServerProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.time.Duration;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
public class OAuth2AccessTokenMemoryImpl implements OAuth2AccessTokenMemory {

    private static final String REDIS_ROOT_KEY = "OAuth2AccessTokenMemory.";
    private static final String REDIS_ACCESS_TOKEN_KEY = REDIS_ROOT_KEY + "accessToken.";
    private static final Duration ACCESS_TOKEN_EXPIRE_TIME = Duration.ofHours(2);

    private final OAuth2ServerProperty property;
    private final RedisTemplate<String, Serializable> redisTemplate;

    @Autowired
    public OAuth2AccessTokenMemoryImpl(OAuth2ServerProperty property, RedisTemplate<String, Serializable> redisTemplate) {
        this.property = property;
        this.redisTemplate = redisTemplate;
    }

    @Override
    public OAuth2AccessTokenDTO.Response generate(OAuth2Triple triple) {
        String accessToken = StringUtils.randomUUID();
        while (Boolean.TRUE.equals(redisTemplate.hasKey(buildKey(accessToken)))) {
            accessToken = StringUtils.randomUUID();
        }

        OAuth2AccessTokenDO memory = new OAuth2AccessTokenDO(triple);
        redisTemplate.opsForValue().set(buildKey(accessToken), memory, property.getEffectiveTime());

        return new OAuth2AccessTokenDTO.Response(accessToken,
                OAuth2Constants.TOKEN_TYPE,
                System.currentTimeMillis() + property.getEffectiveTime().toMillis(),
                memory.getRefreshToken(),
                StringUtils.collectionToString(memory.getScopes(), OAuth2Constants.SCOPES_SEPARATOR)
        );
    }

    @Override
    public UUID verify(String accessToken, String scope) throws OAuth2Exception {
        String key = buildKey(accessToken);
        exist(key);
        OAuth2AccessTokenDO memory = (OAuth2AccessTokenDO)redisTemplate.opsForValue().get(key);
        assert memory != null;
        if (!org.springframework.util.StringUtils.hasText(scope)) {
            // 该操作不需要认证已授权的范围
            return memory.getUserId();
        } else if (memory.getScopes().contains(scope)) {
            // 验证该操作已在授权范围内
            return memory.getUserId();
        }
        throw new OAuth2Exception(OAuth2ErrorEnum.AccessTokenError.INVALID_SCOPE, "用户未授权此操作");
    }

    @Override
    public OAuth2AccessTokenBO refresh(OAuth2AccessTokenBO record) throws OAuth2Exception {
        String oldKey = buildKey(record.getAccessToken());
        exist(oldKey);
        OAuth2AccessTokenDO memory = (OAuth2AccessTokenDO)redisTemplate.opsForValue().get(oldKey);
        assert memory != null;
        if (!memory.getRefreshToken().equals(record.getRefreshToken())) {
            throw new OAuth2Exception(OAuth2ErrorEnum.AccessTokenError.INVALID_GRANT, "RefreshToken错误");
        }

        String accessToken = StringUtils.randomUUID();
        String refreshToken = StringUtils.randomUUID();
        while (Boolean.TRUE.equals(redisTemplate.hasKey(buildKey(accessToken)))) {
            accessToken = StringUtils.randomUUID();
        }
        memory.setRefreshToken(refreshToken);
        redisTemplate.opsForValue().set(buildKey(accessToken), memory, property.getEffectiveTime());
        redisTemplate.delete(oldKey);

        record.setAccessToken(accessToken);
        record.setRefreshToken(refreshToken);
        record.setExpiresIn(System.currentTimeMillis() + property.getEffectiveTime().toMillis());
        return record;
    }

    @Override
    public void revoke(String accessToken) throws OAuth2Exception {
        String key = buildKey(accessToken);
        exist(key);
        redisTemplate.delete(key);
    }

    private void exist(String key) throws OAuth2Exception {
        if (Boolean.FALSE.equals(redisTemplate.hasKey(key))) {
            throw new OAuth2Exception(OAuth2ErrorEnum.AccessTokenError.INVALID_GRANT, "AccessToken已过期");
        }
    }

    private String buildKey(String key) {
        return REDIS_ACCESS_TOKEN_KEY + key;
    }

}
