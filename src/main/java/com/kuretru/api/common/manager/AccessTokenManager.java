package com.kuretru.api.common.manager;

import com.kuretru.api.common.entity.business.AccessTokenBO;
import com.kuretru.api.common.exception.ApiException;
import com.kuretru.api.common.exception.AuthenticationFailedException;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public interface AccessTokenManager {

    /**
     * 登录时创建Token
     *
     * @param model Token model
     */
    void createToken(AccessTokenBO model);

    /**
     * 使用时验证Token
     *
     * @param incomingToken 传入Token
     * @return 传出用户实体
     * @throws ApiException 认证失败异常
     */
    AccessTokenBO verifyToken(String incomingToken) throws ApiException;

    /**
     * 登出时删除Token
     *
     * @param incomingToken 传入Token
     */
    void removeToken(String incomingToken);

}
