package com.kuretru.api.common.manager.impl;

import com.kuretru.api.common.constant.code.UserErrorCodes;
import com.kuretru.api.common.entity.business.AccessTokenBO;
import com.kuretru.api.common.entity.data.AccessTokenDO;
import com.kuretru.api.common.entity.transfer.AccessTokenDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.manager.AccessTokenManager;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class InMemoryTokenAccessManagerImpl implements AccessTokenManager {

    private final ConcurrentMap<String, AccessTokenDO> database;

    public InMemoryTokenAccessManagerImpl() {
        database = new ConcurrentHashMap<>(16);
    }

    @Override
    public AccessTokenDTO generate(UUID userId, Set<String> roles) {
        // 7BF29055-[1C77-4287-8FB2]-D2FB73480856
        String id = UUID.randomUUID().toString().substring(9, 23);
        while (database.containsKey(id)) {
            id = UUID.randomUUID().toString().substring(9, 23);
        }

        AccessTokenDO value = new AccessTokenDO();
        value.setSecret(UUID.randomUUID().toString().replace("-", ""));
        value.setUserId(userId);
        value.setRoles(roles);
        value.setExpireTime(Instant.now().plus(120, ChronoUnit.MINUTES));
        database.put(id, value);

        return new AccessTokenDTO(id, value.getSecret());
    }

    @Override
    public AccessTokenBO get(String id) throws ServiceException {
        exist(id);
        AccessTokenDO value = database.get(id);

        Instant now = Instant.now();
        if (value.getExpireTime().isAfter(now)) {
            database.remove(id);
            throw new ServiceException.Unauthorized(UserErrorCodes.USER_LOGIN_EXPIRED, "登录已过期，请重新登录");
        }

        AccessTokenBO result = new AccessTokenBO();
        result.setSecret(value.getSecret());
        result.setUserId(value.getUserId());
        result.setRoles(value.getRoles());
        return result;
    }

    @Override
    public void refresh(String id) throws ServiceException {
        exist(id);
        AccessTokenDO value = database.get(id);
        value.setExpireTime(Instant.now().plus(120, ChronoUnit.MINUTES));
    }

    @Override
    public void revoke(String id) throws ServiceException {
        exist(id);
        database.remove(id);
    }

    private void exist(String id) throws ServiceException {
        if (!database.containsKey(id)) {
            throw new ServiceException.Unauthorized(UserErrorCodes.ACCESS_PERMISSION_ERROR, "AccessToken不存在");
        }
    }

}
