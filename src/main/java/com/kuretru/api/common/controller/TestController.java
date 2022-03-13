package com.kuretru.api.common.controller;

import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.util.InstantUtils;
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
        String now = InstantUtils.toString(Instant.now());
        return ApiResponse.success(now);
    }

}
