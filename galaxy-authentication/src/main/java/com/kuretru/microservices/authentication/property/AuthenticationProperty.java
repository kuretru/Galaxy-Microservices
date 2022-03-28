package com.kuretru.microservices.authentication.property;

import lombok.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConstructorBinding;
import org.springframework.boot.context.properties.bind.DefaultValue;

import java.time.Duration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Value
@ConstructorBinding
@ConfigurationProperties(prefix = "galaxy.authentication")
public class AuthenticationProperty {

    /**
     * AccessToken的有效时间
     */
    Duration expireTime;

    public AuthenticationProperty(@DefaultValue("PT2H") Duration expireTime) {
        this.expireTime = expireTime;
    }

}
