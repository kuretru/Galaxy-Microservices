package com.kuretru.microservices.oauth2.server.property;

import lombok.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConstructorBinding;
import org.springframework.boot.context.properties.bind.DefaultValue;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Value
@ConstructorBinding
@ConfigurationProperties(prefix = "galaxy.oauth2.server")
public class OAuth2ServerProperty {

    /**
     * 前端地址，用于OAuth2用户授权操作时重定向
     */
    String frontEndUrl;

    public OAuth2ServerProperty(@DefaultValue("") String frontEndUrl) {
        this.frontEndUrl = frontEndUrl;
    }

}
