package com.kuretru.microservices.authentication.configuration;

import com.kuretru.microservices.authentication.aspect.SimpleAuthorizationAspect;
import com.kuretru.microservices.authentication.interceptor.AuthenticationInterceptor;
import com.kuretru.microservices.authentication.manager.AccessTokenManager;
import com.kuretru.microservices.authentication.manager.impl.InMemoryAccessTokenManagerImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
public class AuthenticationAutoConfiguration {

    @Bean
    public AccessTokenManager accessTokenManager() {
        return new InMemoryAccessTokenManagerImpl();
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
