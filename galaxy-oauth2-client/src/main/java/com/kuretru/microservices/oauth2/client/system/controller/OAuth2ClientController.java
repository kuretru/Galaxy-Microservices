package com.kuretru.microservices.oauth2.client.system.controller;

import com.kuretru.microservices.authentication.entity.UserLoginDTO;
import com.kuretru.microservices.oauth2.client.galaxy.manager.GalaxyClientManager;
import com.kuretru.microservices.oauth2.client.system.service.UserService;
import com.kuretru.microservices.oauth2.common.entity.GalaxyUserDTO;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AuthorizeDTO;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@RestController
public class OAuth2ClientController extends BaseOAuth2ClientController {

    private final UserService userService;

    @Autowired
    public OAuth2ClientController(GalaxyClientManager galaxyManager, UserService userService) {
        super(galaxyManager);
        this.userService = userService;
    }

    @Override
    public ApiResponse<?> galaxyCallback(@Validated @RequestBody OAuth2AuthorizeDTO.Response response) throws ServiceException {
        GalaxyUserDTO galaxyUserDTO = galaxyManager.callback(response);
        UserLoginDTO result = userService.login(galaxyUserDTO);
        return ApiResponse.success(result);
    }

}
