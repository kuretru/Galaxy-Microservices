package com.kuretru.api.common.manager.impl;

import com.kuretru.api.common.configuration.CommonProperties;
import com.kuretru.api.common.configuration.GeneralConstants;
import com.kuretru.api.common.entity.business.AccessTokenBO;
import com.kuretru.api.common.exception.ApiException;
import com.kuretru.api.common.exception.AuthenticationFailedException;
import com.kuretru.api.common.manager.AccessTokenManager;
import com.kuretru.api.common.util.StringUtils;
import lombok.NonNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Component
public class AccessTokenManagerImpl extends BaseRedisManagerImpl implements AccessTokenManager {

    private CommonProperties commonProperties;

    @Autowired
    public AccessTokenManagerImpl(StringRedisTemplate redisTemplate, CommonProperties commonProperties) {
        super(redisTemplate);
        this.commonProperties = commonProperties;
    }

    @Async
    @Override
    public void createToken(@NonNull AccessTokenBO model) {
        if (model.getId() == null || model.getRole() == null || StringUtils.isNullOrEmpty(model.getToken())) {
            throw new Error("缺少用户ID或用户角色或用户Token");
        }
        String key = model.getToken();
        String value = model.getIdAndRole();

        // 1.创建新Token
        super.create(key, value);

        // 2.若存在旧反向Token(已在别处登录)，则删除(踢掉线)
        String reverseKey = super.get(value);
        if (!StringUtils.isNullOrEmpty(reverseKey)) {
            super.remove(reverseKey);
        }

        // 3.创建新反向Token
        super.create(value, key);
    }

    @Override
    public AccessTokenBO verifyToken(@NonNull String incomingToken) throws ApiException {
        String idAndRole = super.get(incomingToken);

        // 1.验证Token是否合法
        if (StringUtils.isNullOrEmpty(idAndRole)) {
            throw new AuthenticationFailedException("Token非法或已过期");
        }

        // 2.可省略步骤，双向验证是否合法
        String redisToken = super.get(idAndRole);
        if (StringUtils.isNullOrEmpty(redisToken)) {
            throw ApiException.unknownException();
        } else if (!redisToken.equals(incomingToken)) {
            throw new AuthenticationFailedException("已在别的设备上登录");
        }

        // 3.刷新过期时间
        super.update(incomingToken);
        super.update(idAndRole);

        return AccessTokenBO.fromRedis(idAndRole, incomingToken);
    }

    @Async
    @Override
    public void removeToken(@NonNull String incomingToken) {
        String value = get(incomingToken);
        if (!StringUtils.isNullOrEmpty(value)) {
            super.remove(incomingToken);
            super.remove(value);
        }
    }

    @Override
    protected String getKeyName(String rawName) {
        return commonProperties.getProjectPrefix() + GeneralConstants.ACCESS_TOKEN_KEY + rawName;
    }

}
