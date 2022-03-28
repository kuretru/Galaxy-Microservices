package com.kuretru.microservices.authentication.configuration;

import com.kuretru.microservices.authentication.aspect.SimpleAuthorizationAspect;
import com.kuretru.microservices.authentication.interceptor.AuthenticationInterceptor;
import com.kuretru.microservices.authentication.manager.AccessTokenManager;
import com.kuretru.microservices.authentication.manager.impl.InMemoryAccessTokenManagerImpl;
import com.kuretru.microservices.authentication.property.AuthenticationProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
@EnableConfigurationProperties(AuthenticationProperty.class)
public class AuthenticationAutoConfiguration {

    @Bean
    @Autowired
    public AccessTokenManager accessTokenManager(AuthenticationProperty property) {
        return new InMemoryAccessTokenManagerImpl(property);
    }

    @Bean
    public AuthenticationInterceptor authenticationInterceptor() {
        return new AuthenticationInterceptor();
    }

    @Bean
    @Autowired
    public SimpleAuthorizationAspect authorizationAspect(AccessTokenManager accessTokenManager) {
        return new SimpleAuthorizationAspect(accessTokenManager);
    }

}
