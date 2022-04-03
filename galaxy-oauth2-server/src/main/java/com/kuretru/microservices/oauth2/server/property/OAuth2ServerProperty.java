package com.kuretru.microservices.oauth2.server.property;

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
@ConfigurationProperties(prefix = "galaxy.oauth2.server")
public class OAuth2ServerProperty {

    /**
     * 前端地址，用于OAuth2用户授权操作时重定向
     */
    String frontEndUrl;

    /**
     * 身份认证需要在该时间内完成
     */
    Duration expireTime;

    /**
     * 身份认证完成后有效的时间
     */
    Duration effectiveTime;

    public OAuth2ServerProperty(@DefaultValue("") String frontEndUrl,
                                @DefaultValue("PT15M") Duration expireTime,
                                @DefaultValue("PT2H") Duration effectiveTime) {
        this.frontEndUrl = frontEndUrl;
        this.expireTime = expireTime;
        this.effectiveTime = effectiveTime;
    }

}
