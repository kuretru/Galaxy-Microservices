package com.kuretru.microservices.oauth2.client.memory.impl;

import com.kuretru.microservices.oauth2.client.memory.OAuth2StateMemory;
import com.kuretru.microservices.oauth2.client.property.OAuth2ClientProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
@ConditionalOnBean(StringRedisTemplate.class)
public class RedisOAuth2StateMemoryImpl implements OAuth2StateMemory {

    private static final String REDIS_ROOT_KEY = "OAuth2StateMemory.";
    private static final String REDIS_STATE_KEY = REDIS_ROOT_KEY + "state.";

    private final OAuth2ClientProperty property;
    private final RedisTemplate<String, String> redisTemplate;

    @Autowired
    public RedisOAuth2StateMemoryImpl(OAuth2ClientProperty property, RedisTemplate<String, String> redisTemplate) {
        this.property = property;
        this.redisTemplate = redisTemplate;
    }

    @Override
    public String generateAndSave(String redirectUri) {
        String state = UUID.randomUUID().toString();
        while (Boolean.TRUE.equals(redisTemplate.hasKey(buildKey(state)))) {
            state = UUID.randomUUID().toString();
        }
        redisTemplate.opsForValue().set(buildKey(state), redirectUri, property.getGemini().getStateExpireTime());
        return state;
    }

    @Override
    public String getAndDelete(String state) {
        String key = buildKey(state);
        if (Boolean.FALSE.equals(redisTemplate.hasKey(key))) {
            return null;
        }
        return redisTemplate.opsForValue().getAndDelete(key);
    }

    private String buildKey(String key) {
        return REDIS_STATE_KEY + key;
    }

}
