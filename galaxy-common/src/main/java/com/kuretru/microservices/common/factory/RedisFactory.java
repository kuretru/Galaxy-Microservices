package com.kuretru.microservices.common.factory;

import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.GenericJacksonJsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import java.io.Serializable;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class RedisFactory {

    public static RedisTemplate<String, Serializable> serializableRedisTemplate(LettuceConnectionFactory connectionFactory) {
        RedisTemplate<String, Serializable> redisTemplate = new RedisTemplate<>();
        redisTemplate.setKeySerializer(new StringRedisSerializer());
        redisTemplate.setValueSerializer(GenericJacksonJsonRedisSerializer.create(builder -> {
        }));
        redisTemplate.setConnectionFactory(connectionFactory);
        return redisTemplate;
    }

}
