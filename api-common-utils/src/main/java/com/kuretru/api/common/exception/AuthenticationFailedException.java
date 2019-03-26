package com.kuretru.api.common.exception;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class AuthenticationFailedException extends ApiException {

    public AuthenticationFailedException(String message) {
        super(message);
    }

}
