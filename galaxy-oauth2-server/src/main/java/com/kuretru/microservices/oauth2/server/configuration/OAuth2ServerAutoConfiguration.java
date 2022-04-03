package com.kuretru.microservices.oauth2.server.configuration;

import com.kuretru.microservices.oauth2.server.property.OAuth2ServerProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
@ComponentScan("com.kuretru.microservices.oauth2.server")
@EnableConfigurationProperties(OAuth2ServerProperty.class)
public class OAuth2ServerAutoConfiguration {

}
