package com.kuretru.microservices.authentication.configuration;

import com.kuretru.microservices.authentication.property.AuthenticationProperty;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.ComponentScan;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@AutoConfiguration
@ComponentScan("com.kuretru.microservices.authentication")
@EnableConfigurationProperties(AuthenticationProperty.class)
public class AuthenticationAutoConfiguration {


}
