package com.kuretru.microservices.oauth2.client.manager;

import com.kuretru.microservices.oauth2.client.entity.OAuth2AuthorizeRequestDTO;

/**
 * OAuth2 Client接口
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface OAuth2ClientManager {

    /**
     * 向服务端请求身份验证
     *
     * @param record 请求实体
     * @return 重定向至服务端的URL
     */
    String authorize(OAuth2AuthorizeRequestDTO record);

}
