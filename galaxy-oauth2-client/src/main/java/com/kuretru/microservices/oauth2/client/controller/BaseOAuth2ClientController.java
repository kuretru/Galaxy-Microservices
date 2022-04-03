package com.kuretru.microservices.oauth2.client.controller;

import com.kuretru.microservices.oauth2.client.entity.OAuth2AuthorizeRequestDTO;
import com.kuretru.microservices.oauth2.client.manager.OAuth2ClientManager;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AuthorizeDTO;
import com.kuretru.microservices.web.controller.BaseController;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@RequestMapping("/oauth2")
public abstract class BaseOAuth2ClientController extends BaseController {

    protected final OAuth2ClientManager galaxyManager;

    public BaseOAuth2ClientController(OAuth2ClientManager galaxyManager) {
        this.galaxyManager = galaxyManager;
    }

    @PostMapping("/galaxy/authorize")
    public ApiResponse<?> galaxyAuthorize(@RequestBody OAuth2AuthorizeRequestDTO record) throws ServiceException {
        String redirectUrl = galaxyManager.authorize(record);
        return ApiResponse.success(redirectUrl);
    }

    /**
     * Galaxy OAuth2服务器回调方法
     *
     * @param response 回调内容
     * @return 响应实体
     * @throws ServiceException 业务异常
     */
    @GetMapping("/galaxy/callback")
    public abstract ApiResponse<?> galaxyCallback(OAuth2AuthorizeDTO.Response response) throws ServiceException;

}
