package com.kuretru.microservices.oauth2.client.configuration;

import com.kuretru.microservices.oauth2.client.property.OAuth2ClientProperty;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.data.redis.RedisAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
@ComponentScan("com.kuretru.microservices.oauth2.client")
@EnableConfigurationProperties(OAuth2ClientProperty.class)
@AutoConfigureAfter(RedisAutoConfiguration.class)
public class OAuth2ClientAutoConfiguration {

}
