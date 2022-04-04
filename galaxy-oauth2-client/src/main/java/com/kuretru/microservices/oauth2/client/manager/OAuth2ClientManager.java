package com.kuretru.microservices.oauth2.client.manager;

import com.kuretru.microservices.oauth2.client.entity.OAuth2AuthorizeRequestDTO;
import com.kuretru.microservices.oauth2.common.entity.GalaxyUserDTO;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AuthorizeDTO;
import com.kuretru.microservices.web.exception.ServiceException;

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

    /**
     * 服务端身份验证成功后回调该方法，然后向服务端请求AccessToken
     *
     * @param response 回调内容
     * @return 用户信息实体
     * @throws ServiceException 业务异常
     */
    GalaxyUserDTO callback(OAuth2AuthorizeDTO.Response response) throws ServiceException;

}
