package com.kuretru.microservices.oauth2.client.controller;

import com.kuretru.microservices.oauth2.client.entity.OAuth2AuthorizeRequestDTO;
import com.kuretru.microservices.oauth2.client.manager.OAuth2ClientManager;
import com.kuretru.microservices.web.constant.code.ServiceErrorCodes;
import com.kuretru.microservices.web.controller.BaseController;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@RestController
@RequestMapping("/oauth2")
public class OAuth2ClientController extends BaseController {

    private final OAuth2ClientManager galaxyManager;

    @Autowired
    public OAuth2ClientController(@Qualifier("galaxyOAuth2ClientManagerImpl") OAuth2ClientManager galaxyManager) {
        this.galaxyManager = galaxyManager;
    }

    @PostMapping("/galaxy/authorize")
    public ApiResponse<?> galaxyAuthorize(@RequestBody OAuth2AuthorizeRequestDTO record) throws ServiceException {
        String redirectUrl = galaxyManager.authorize(record);
        try {
            response.sendRedirect(redirectUrl);
        } catch (IOException e) {
            throw ServiceException.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "未能重定向");
        }
        return ApiResponse.success("重定向中......");
    }

}
