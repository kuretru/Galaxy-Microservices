package com.kuretru.microservices.oauth2.client.galaxy;

import com.kuretru.microservices.common.factory.RedisFactory;
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
@ComponentScan("com.kuretru.microservices.oauth2.client.galaxy")
@EnableConfigurationProperties(GalaxyClientProperty.class)
@AutoConfigureAfter(RedisAutoConfiguration.class)
public class GalaxyClientAutoConfiguration {

    @Bean("serializableRedisTemplate")
    @ConditionalOnBean(StringRedisTemplate.class)
    @ConditionalOnMissingBean(name = {"serializableRedisTemplate"})
    public RedisTemplate<String, Serializable> serializableRedisTemplate(LettuceConnectionFactory connectionFactory) {
        return RedisFactory.serializableRedisTemplate(connectionFactory);
    }

}
