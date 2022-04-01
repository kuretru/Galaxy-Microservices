package com.kuretru.microservices.oauth2.client.manager.impl;

import com.kuretru.microservices.oauth2.client.manager.OAuth2ClientStateManager;
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
public class RedisOAuth2ClientStateManagerImpl implements OAuth2ClientStateManager {

    private static final String REDIS_ROOT_KEY = "RedisOAuth2ClientStateManager.";
    private static final String REDIS_STATE_KEY = REDIS_ROOT_KEY + "state.";

    private final OAuth2ClientProperty property;
    private final RedisTemplate<String, String> redisTemplate;

    @Autowired
    public RedisOAuth2ClientStateManagerImpl(OAuth2ClientProperty property, RedisTemplate<String, String> redisTemplate) {
        this.property = property;
        this.redisTemplate = redisTemplate;
    }

    @Override
    public String generateAndSave(String redirectUri) {
        String state = UUID.randomUUID().toString();
        String key = buildKey(state);
        while (Boolean.TRUE.equals(redisTemplate.hasKey(key))) {
            state = UUID.randomUUID().toString();
        }
        redisTemplate.opsForValue().set(key, redirectUri, property.getGemini().getStateExpireTime());
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
