package com.kuretru.microservices.oauth2.client.system.controller;

import com.kuretru.microservices.authentication.annotaion.RequireAuthorization;
import com.kuretru.microservices.authentication.context.AccessTokenContext;
import com.kuretru.microservices.authentication.entity.AccessTokenDTO;
import com.kuretru.microservices.common.constant.EmptyConstants;
import com.kuretru.microservices.oauth2.client.system.entity.UserDTO;
import com.kuretru.microservices.oauth2.client.system.service.UserService;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.controller.BaseController;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@RestController
@RequestMapping("/users")
public class UserController extends BaseController {

    private final UserService service;

    @Autowired
    public UserController(UserService service) {
        this.service = service;
    }

    @GetMapping("/{id}")
    @RequireAuthorization
    public ApiResponse<?> get(@PathVariable("id") UUID id) throws ServiceException {
        if (id == null || EmptyConstants.EMPTY_UUID.equals(id)) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        }
        if (!id.equals(AccessTokenContext.getUserId())) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "请勿操作别人的数据");
        }
        UserDTO result = service.get(id);
        if (null == result) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        }
        return ApiResponse.success(result);
    }

    @PostMapping("/logout")
    @RequireAuthorization
    public ApiResponse<?> logout(@RequestBody AccessTokenDTO accessToken) {
        service.logout(accessToken.getId());
        return ApiResponse.success("已退出登录...");
    }

}
