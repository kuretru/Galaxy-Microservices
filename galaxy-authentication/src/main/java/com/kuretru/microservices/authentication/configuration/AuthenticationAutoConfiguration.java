package com.kuretru.microservices.authentication.configuration;

import com.kuretru.microservices.authentication.aspect.SimpleAuthorizationAspect;
import com.kuretru.microservices.authentication.manager.AccessTokenManager;
import com.kuretru.microservices.authentication.property.AuthenticationProperty;
import com.kuretru.microservices.common.factory.RedisFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.data.redis.RedisAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.io.Serializable;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
@ComponentScan("com.kuretru.microservices.authentication")
@EnableConfigurationProperties(AuthenticationProperty.class)
@AutoConfigureAfter(RedisAutoConfiguration.class)
public class AuthenticationAutoConfiguration {

    @Bean("serializableRedisTemplate")
    @ConditionalOnBean(StringRedisTemplate.class)
    @ConditionalOnMissingBean(name = {"serializableRedisTemplate"})
    public RedisTemplate<String, Serializable> serializableRedisTemplate(LettuceConnectionFactory connectionFactory) {
        return RedisFactory.serializableRedisTemplate(connectionFactory);
    }

    @Bean
    @Autowired
    public SimpleAuthorizationAspect authorizationAspect(AccessTokenManager accessTokenManager) {
        return new SimpleAuthorizationAspect(accessTokenManager);
    }

}
