package com.kuretru.microservices.oauth2.client.github;

import lombok.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.ConstructorBinding;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */

@Value
@ConstructorBinding
@ConfigurationProperties(prefix = "galaxy.oauth2.client.github")
public class GithubClientProperty {

    /**
     * 在Github申请得到的ClientID
     */
    String clientId;

    /**
     * 在Github申请得到的ClientSecret
     */
    String clientSecret;

}
