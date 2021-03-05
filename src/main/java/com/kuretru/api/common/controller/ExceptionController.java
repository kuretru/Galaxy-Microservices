package com.kuretru.api.common.controller;

import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.exception.ServiceException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * 统一异常处理控制器
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@ControllerAdvice
@ResponseBody
public class ExceptionController {

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(ServiceException.BadRequest.class)
    public ApiResponse<?> badRequestHandler(ServiceException.BadRequest e) {
        return ApiResponse.build(e.getCode(), e.getMessage());
    }

    @ResponseStatus(HttpStatus.UNAUTHORIZED)
    @ExceptionHandler(ServiceException.Unauthorized.class)
    public ApiResponse<?> unauthorizedHandler(ServiceException.Unauthorized e) {
        return ApiResponse.build(e.getCode(), e.getMessage());
    }

    @ResponseStatus(HttpStatus.FORBIDDEN)
    @ExceptionHandler(ServiceException.Forbidden.class)
    public ApiResponse<?> forbiddenHandler(ServiceException.Forbidden e) {
        return ApiResponse.build(e.getCode(), e.getMessage());
    }

    @ResponseStatus(HttpStatus.NOT_FOUND)
    @ExceptionHandler(ServiceException.NotFound.class)
    public ApiResponse<?> notFoundHandler(ServiceException.NotFound e) {
        return ApiResponse.build(e.getCode(), e.getMessage());
    }


    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    @ExceptionHandler(ServiceException.InternalServerError.class)
    public ApiResponse<?> internalServerErrorHandler(ServiceException.InternalServerError e) {
        return ApiResponse.build(e.getCode(), e.getMessage());
    }

}