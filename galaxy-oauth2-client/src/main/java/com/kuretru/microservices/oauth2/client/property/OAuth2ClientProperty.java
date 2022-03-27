package com.kuretru.microservices.oauth2.client.property;

import lombok.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConstructorBinding;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Value
@ConstructorBinding
@ConfigurationProperties(prefix = "galaxy.oauth2.client")
public class OAuth2ClientProperty {

    /**
     * 在OAuth2服务器申请得到的应用ID
     */
    String applicationId;
    /**
     * 在OAuth2服务器申请得到的应用密钥
     */
    String applicationSecret;

}
