package com.kuretru.api.common.controller;

import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.util.InstantUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.time.Instant;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@RestController
@RequestMapping("/api")
public class TestController extends BaseController {

    public ApiResponse<?> ping() {
        String now = InstantUtils.toString(Instant.now());
        return ApiResponse.success(now);
    }

}
