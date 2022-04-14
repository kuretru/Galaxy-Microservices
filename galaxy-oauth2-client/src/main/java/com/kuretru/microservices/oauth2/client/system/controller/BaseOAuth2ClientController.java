package com.kuretru.microservices.oauth2.client.system.controller;

import com.kuretru.microservices.oauth2.client.galaxy.entity.GalaxyAuthorizeRequestDTO;
import com.kuretru.microservices.oauth2.client.galaxy.manager.GalaxyClientManager;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AuthorizeDTO;
import com.kuretru.microservices.web.controller.BaseController;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@RequestMapping("/oauth2")
public abstract class BaseOAuth2ClientController extends BaseController {

    protected final GalaxyClientManager galaxyManager;

    public BaseOAuth2ClientController(GalaxyClientManager galaxyManager) {
        this.galaxyManager = galaxyManager;
    }

    @PostMapping("/galaxy/authorize")
    public ApiResponse<?> galaxyAuthorize(@RequestBody GalaxyAuthorizeRequestDTO record) throws ServiceException {
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
    @PostMapping("/galaxy/callback")
    public abstract ApiResponse<?> galaxyCallback(@Validated @RequestBody OAuth2AuthorizeDTO.Response response) throws ServiceException;

}
