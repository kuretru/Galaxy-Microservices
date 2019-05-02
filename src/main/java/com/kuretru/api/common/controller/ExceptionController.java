package com.kuretru.api.common.controller;

import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.exception.*;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@ControllerAdvice
@ResponseBody
public class ExceptionController extends BaseController {

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(ApiException.class)
    public ApiResponse apiExceptionHandler(ApiException e) {
        return ApiResponse.error(e.getMessage());
    }

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(MissingParametersException.class)
    public ApiResponse missingParametersExceptionHandler(MissingParametersException e) {
        return ApiResponse.missingParameters(e.getMessage());
    }

    @ResponseStatus(HttpStatus.BAD_REQUEST)
    @ExceptionHandler(InvalidParametersException.class)
    public ApiResponse invalidParametersExceptionHandler(InvalidParametersException e) {
        return ApiResponse.invalidParameters(e.getMessage());
    }

    @ResponseStatus(HttpStatus.NOT_FOUND)
    @ExceptionHandler(NotFoundException.class)
    public ApiResponse notFoundExceptionHandler(NotFoundException e) {
        return ApiResponse.notFound(e.getMessage());
    }

    @ResponseStatus(HttpStatus.UNAUTHORIZED)
    @ExceptionHandler(AuthenticationFailedException.class)
    public ApiResponse authenticationFailedExceptionHandler(AuthenticationFailedException e) {
        return ApiResponse.unauthorized(e.getMessage());
    }

}
