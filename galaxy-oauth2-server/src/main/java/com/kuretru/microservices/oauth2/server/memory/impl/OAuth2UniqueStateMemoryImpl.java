package com.kuretru.microservices.oauth2.server.memory.impl;

import com.kuretru.microservices.oauth2.server.memory.OAuth2UniqueStateMemory;
import com.kuretru.microservices.oauth2.server.property.OAuth2ServerProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
public class OAuth2UniqueStateMemoryImpl implements OAuth2UniqueStateMemory {

    private static final String REDIS_ROOT_KEY = "OAuth2UniqueStateMemory.";
    private static final String REDIS_STATE_KEY = REDIS_ROOT_KEY + "state.";

    private final OAuth2ServerProperty property;
    private final StringRedisTemplate redisTemplate;

    @Autowired
    public OAuth2UniqueStateMemoryImpl(OAuth2ServerProperty property, StringRedisTemplate redisTemplate) {
        this.property = property;
        this.redisTemplate = redisTemplate;
    }

    @Override
    public boolean exist(String state) {
        String key = buildKey(state);
        if (Boolean.TRUE.equals(redisTemplate.hasKey(key))) {
            return true;
        }
        redisTemplate.opsForValue().set(key, state, property.getExpireTime());
        return false;
    }

    private String buildKey(String key) {
        return REDIS_STATE_KEY + key;
    }

}
