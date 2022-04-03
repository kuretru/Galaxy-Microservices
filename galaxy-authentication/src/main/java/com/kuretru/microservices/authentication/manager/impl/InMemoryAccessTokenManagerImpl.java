package com.kuretru.microservices.authentication.manager.impl;

import com.kuretru.microservices.authentication.entity.AccessTokenBO;
import com.kuretru.microservices.authentication.entity.AccessTokenDTO;
import com.kuretru.microservices.authentication.manager.AccessTokenManager;
import com.kuretru.microservices.authentication.property.AuthenticationProperty;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.exception.ServiceException;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
@ConditionalOnMissingBean(StringRedisTemplate.class)
public class InMemoryAccessTokenManagerImpl implements AccessTokenManager {

    private final AuthenticationProperty property;
    private final ConcurrentMap<String, AccessTokenDO> database;

    public InMemoryAccessTokenManagerImpl(AuthenticationProperty property) {
        this.property = property;
        database = new ConcurrentHashMap<>(16);
    }

    @Override
    public AccessTokenDTO generate(UUID userId, Set<String> roles) {
        // 7BF29055-[1C77-4287-8FB2]-D2FB73480856
        String id = UUID.randomUUID().toString().substring(9, 23);
        while (database.containsKey(id)) {
            id = UUID.randomUUID().toString().substring(9, 23);
        }

        AccessTokenDO value = new AccessTokenDO(
                UUID.randomUUID().toString().replace("-", ""),
                userId,
                roles,
                Instant.now().plus(property.getExpireTime())
        );
        database.put(id, value);

        return new AccessTokenDTO(id, value.getSecret());
    }

    @Override
    public AccessTokenBO get(String id) throws ServiceException {
        exist(id);
        AccessTokenDO value = database.get(id);

        if (Instant.now().isAfter(value.getExpireTime())) {
            database.remove(id);
            throw ServiceException.build(UserErrorCodes.USER_LOGIN_EXPIRED, "登录已过期，请重新登录");
        }

        return new AccessTokenBO(value.getSecret(), value.getUserId(), value.getRoles());
    }

    @Override
    public void refresh(String id) throws ServiceException {
        exist(id);
        AccessTokenDO value = database.get(id);
        value.setExpireTime(Instant.now().plus(property.getExpireTime()));
    }

    @Override
    public void revoke(String id) throws ServiceException {
        exist(id);
        database.remove(id);
    }

    private void exist(String id) throws ServiceException {
        if (!database.containsKey(id)) {
            throw ServiceException.build(UserErrorCodes.ACCESS_PERMISSION_ERROR, "AccessToken不存在");
        }
    }

    @Data
    @EqualsAndHashCode(callSuper = true)
    @ToString(callSuper = true)
    @NoArgsConstructor
    private static class AccessTokenDO extends com.kuretru.microservices.authentication.entity.AccessTokenDO {

        private Instant expireTime;

        public AccessTokenDO(String secret, UUID userId, Set<String> roles, Instant expireTime) {
            super(secret, userId, roles);
            this.expireTime = expireTime;
        }

    }

}
