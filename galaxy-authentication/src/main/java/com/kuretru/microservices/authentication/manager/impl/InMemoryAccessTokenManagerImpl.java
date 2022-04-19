package com.kuretru.microservices.authentication.manager.impl;

import com.kuretru.microservices.authentication.entity.AccessTokenBO;
import com.kuretru.microservices.authentication.entity.AccessTokenDO;
import com.kuretru.microservices.authentication.entity.AccessTokenDTO;
import com.kuretru.microservices.authentication.manager.AccessTokenManager;
import com.kuretru.microservices.authentication.property.AuthenticationProperty;
import com.kuretru.microservices.common.storage.InMemoryCounterStorage;
import com.kuretru.microservices.common.storage.InMemoryStorage;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.exception.ServiceException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.Set;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Slf4j
@Service
@ConditionalOnMissingBean(StringRedisTemplate.class)
public class InMemoryAccessTokenManagerImpl implements AccessTokenManager {

    private static final int PURGE_INTERVAL = 128;
    private final AuthenticationProperty property;
    private final InMemoryStorage<String, AccessTokenDO> database;

    public InMemoryAccessTokenManagerImpl(AuthenticationProperty property) {
        this.property = property;
        database = new InMemoryCounterStorage<>(PURGE_INTERVAL);
    }

    @Override
    public AccessTokenDTO generate(UUID userId, Set<String> roles) {
        // 7BF29055-[1C77-4287-8FB2]-D2FB73480856
        String id = UUID.randomUUID().toString().substring(9, 23);
        while (database.hasKey(id)) {
            id = UUID.randomUUID().toString().substring(9, 23);
        }

        AccessTokenDO value = new AccessTokenDO(
                UUID.randomUUID().toString().replace("-", ""),
                userId,
                roles
        );
        database.set(id, value, property.getExpireTime());
        if (log.isInfoEnabled()) {
            log.info("已保存用户AccessToken凭据");
        }

        return new AccessTokenDTO(id, value.getSecret());
    }

    @Override
    public AccessTokenBO get(String id) throws ServiceException {
        AccessTokenDO value = database.getAndExpire(id, property.getExpireTime());
        if (value == null) {
            throw ServiceException.build(UserErrorCodes.ACCESS_PERMISSION_ERROR, "登录已过期，请重新登录");
        }
        return new AccessTokenBO(value.getSecret(), value.getUserId(), value.getRoles());
    }

    @Override
    public void refresh(String id) throws ServiceException {
        exist(id);
        database.expire(id, property.getExpireTime());
    }

    @Override
    public void revoke(String id) throws ServiceException {
        exist(id);
        database.delete(id);
    }

    private void exist(String id) throws ServiceException {
        if (!database.hasKey(id)) {
            throw ServiceException.build(UserErrorCodes.ACCESS_PERMISSION_ERROR, "登录已过期，请重新登录");
        }
    }

}
