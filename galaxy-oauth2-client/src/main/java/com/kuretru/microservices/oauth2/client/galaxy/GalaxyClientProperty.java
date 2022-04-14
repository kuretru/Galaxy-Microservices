package com.kuretru.microservices.oauth2.client.galaxy;

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
@ConfigurationProperties(prefix = "galaxy.oauth2.client.galaxy")
public class GalaxyClientProperty {

    /**
     * 服务端URL
     */
    String serverUrl;

    /**
     * 在OAuth2服务器申请得到的应用ID
     */
    String applicationId;

    /**
     * 在OAuth2服务器申请得到的应用密钥
     */
    String applicationSecret;

    /**
     * State过期时间
     */
    Duration stateExpireTime;

    public GalaxyClientProperty(String serverUrl, String applicationId, String applicationSecret,
                                @DefaultValue("PT15M") Duration stateExpireTime) {
        this.serverUrl = serverUrl;
        this.applicationId = applicationId;
        this.applicationSecret = applicationSecret;
        this.stateExpireTime = stateExpireTime;
    }

}
