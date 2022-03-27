package com.kuretru.microservices.oauth2.client.configuration;

import com.kuretru.microservices.oauth2.client.property.OAuth2ClientProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
@EnableConfigurationProperties(OAuth2ClientProperty.class)
public class OAuth2ClientAutoConfiguration {

}
