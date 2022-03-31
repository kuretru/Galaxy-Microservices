package com.kuretru.microservices.oauth2.server.manager;

import com.kuretru.microservices.oauth2.common.entity.OAuth2AccessTokenDTO;
import com.kuretru.microservices.oauth2.common.entity.OAuth2Triple;
import com.kuretru.microservices.oauth2.common.exception.OAuth2Exception;
import com.kuretru.microservices.oauth2.server.entity.OAuth2AccessTokenBO;

import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface OAuth2AccessTokenManager {

    /**
     * 根据OAuth2基本三元组，生成AccessToken响应实体，存入数据库
     *
     * @param triple OAuth2基本三元组
     * @return AccessToken响应实体
     */
    OAuth2AccessTokenDTO.Response generate(OAuth2Triple triple);

    /**
     * 验证AccessToken
     *
     * @param accessToken AccessToken
     * @param scope       权限
     * @return 用户ID
     * @throws OAuth2Exception OAuth2异常
     */
    UUID verify(String accessToken, String scope) throws OAuth2Exception;

    /**
     * 刷新AccessToken
     *
     * @param record AccessToken和RefreshToken
     * @return 刷新后的AccessToken和RefreshToken
     * @throws OAuth2Exception OAuth2异常
     */
    OAuth2AccessTokenBO refresh(OAuth2AccessTokenBO record) throws OAuth2Exception;

    /**
     * 吊销AccessToken，从数据库中删除
     *
     * @param accessToken AccessToken
     * @throws OAuth2Exception OAuth2异常
     */
    void revoke(String accessToken) throws OAuth2Exception;

}
