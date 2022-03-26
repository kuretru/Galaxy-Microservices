package com.kuretru.microservices.oauth2.common.exception;

import com.kuretru.microservices.oauth2.common.entity.OAuth2ErrorEnum;
import lombok.Getter;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class OAuth2Exception extends Exception {

    @Getter
    private final OAuth2ErrorEnum errorEnum;

    public OAuth2Exception(OAuth2ErrorEnum errorEnum, String message) {
        super(message);
        this.errorEnum = errorEnum;
    }

}
