package com.kuretru.microservices.oauth2.common.exception;

import com.kuretru.microservices.oauth2.common.entity.OAuth2ErrorEnum;
import com.kuretru.microservices.oauth2.common.entity.OAuth2ErrorResponse;
import lombok.Getter;
import org.springframework.util.StringUtils;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class OAuth2Exception extends Exception {

    private static final String PARAM_PREFIX = "?";

    @Getter
    private final OAuth2ErrorEnum errorEnum;
    @Getter
    private final String errorUrl;
    private final String redirectUrl;

    public OAuth2Exception(OAuth2ErrorEnum errorEnum, String message) {
        super(message);
        this.errorEnum = errorEnum;
        this.redirectUrl = "/";
        this.errorUrl = null;
    }

    public OAuth2Exception(OAuth2ErrorEnum errorEnum, String message, String redirectUrl) {
        super(message);
        this.errorEnum = errorEnum;
        this.redirectUrl = redirectUrl;
        this.errorUrl = null;
    }

    public OAuth2Exception(OAuth2ErrorEnum errorEnum, String message, String redirectUrl, String errorUrl) {
        super(message);
        this.errorEnum = errorEnum;
        this.redirectUrl = redirectUrl;
        this.errorUrl = errorUrl;
    }

    public String toRedirectUrl() {
        if (!StringUtils.hasText(this.redirectUrl)) {
            throw new IllegalArgumentException("重定向地址未赋值");
        }
        StringBuilder result = new StringBuilder(redirectUrl);
        if (!redirectUrl.contains(PARAM_PREFIX)) {
            result.append(PARAM_PREFIX);
        }
        result.append("&error=").append(errorEnum.getValue());
        result.append("&error_description=").append(getMessage());
        if (StringUtils.hasText(this.errorUrl)) {
            result.append("&error_uri=").append(this.getErrorUrl());
        }
        return URLEncoder.encode(result.toString(), StandardCharsets.UTF_8);
    }

    public OAuth2ErrorResponse toResponseEntity() {
        return new OAuth2ErrorResponse(this.errorEnum, this.getMessage(), this.errorUrl);
    }

}
