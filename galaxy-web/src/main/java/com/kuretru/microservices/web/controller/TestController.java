package com.kuretru.microservices.web.controller;

import com.kuretru.microservices.web.constant.code.ServiceErrorCodes;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import java.time.Instant;

/**
 * 测试控制器，提供一些测试功能
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@RestController
@Tag(name = "TestController", description = "测试控制器")
public class TestController extends BaseController {

    @GetMapping("/ping")
    @Operation(summary = "测试与API服务器的连通性")
    public ApiResponse<?> ping() {
        return ApiResponse.success(Instant.now());
    }

    @GetMapping("/exception")
    @Operation(summary = "主动抛出一个异常，测试前端异常处理能力")
    public ApiResponse<?> exception() {
        throw new ServiceException(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "主动抛出异常");
    }

}
