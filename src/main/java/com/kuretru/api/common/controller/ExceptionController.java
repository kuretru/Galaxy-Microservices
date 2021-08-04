package com.kuretru.api.common.controller;

import com.kuretru.api.common.constant.code.ServiceErrorCodes;
import com.kuretru.api.common.constant.code.UserErrorCodes;
import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.exception.ServiceException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

import java.util.List;

/**
 * 统一异常处理控制器
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@ControllerAdvice
@ResponseBody
@Slf4j
public class ExceptionController {

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ApiResponse<?> x(MethodArgumentNotValidException e) {
        StringBuilder stringBuilder = new StringBuilder();
        List<FieldError> fieldErrors = e.getBindingResult().getFieldErrors();
        for (int i = 0; i < fieldErrors.size(); i++) {
            FieldError fieldError = fieldErrors.get(i);
            stringBuilder.append(fieldError.getObjectName()).append('.');
            stringBuilder.append(fieldError.getField()).append(':');
            stringBuilder.append(fieldError.getDefaultMessage()).append((i + 1 < fieldErrors.size()) ? "; " : "");
        }
        return ApiResponse.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, stringBuilder.toString());
    }

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


    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    @ExceptionHandler(Exception.class)
    public ApiResponse<?> exceptionHandler(Exception e) {
        log.error("{}: {}", e.getClass().getSimpleName(), e.getMessage());
        return ApiResponse.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, e.getMessage());
    }

    @ResponseStatus(HttpStatus.INTERNAL_SERVER_ERROR)
    @ExceptionHandler(Error.class)
    public ApiResponse<?> errorHandler(Error e) {
        log.error("{}: {}", e.getClass().getSimpleName(), e.getMessage());
        return ApiResponse.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, e.getMessage());
    }

}