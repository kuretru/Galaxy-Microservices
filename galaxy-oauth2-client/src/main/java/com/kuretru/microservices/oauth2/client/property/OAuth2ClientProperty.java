package com.kuretru.microservices.oauth2.client.property;

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
@ConfigurationProperties(prefix = "galaxy.oauth2.client")
public class OAuth2ClientProperty {

    /**
     * 基于银河微服务的OAuth2客户端
     */
    GeminiClientProperty gemini;

    GithubClientProperty github;

    @Value
    @ConstructorBinding
    public static class GeminiClientProperty {

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

        public GeminiClientProperty(String serverUrl, String applicationId, String applicationSecret,
                                    @DefaultValue("PT15M") Duration stateExpireTime) {
            this.serverUrl = serverUrl;
            this.applicationId = applicationId;
            this.applicationSecret = applicationSecret;
            this.stateExpireTime = stateExpireTime;
        }

    }

    @Value
    @ConstructorBinding
    public static class GithubClientProperty {

        /**
         * 在Github申请得到的ClientID
         */
        String clientId;

        /**
         * 在Github申请得到的ClientSecret
         */
        String clientSecret;

    }

}
