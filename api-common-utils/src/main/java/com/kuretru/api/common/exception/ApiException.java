package com.kuretru.api.common.exception;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class ApiException extends Exception {

    public ApiException(String message) {
        super(message);
    }

    public static ApiException unknownException() {
        return new ApiException("未知异常，请联系管理员");
    }

}
