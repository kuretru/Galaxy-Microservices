package com.kuretru.microservices.oauth2.server.memory.impl;

import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AuthorizeDTO;
import com.kuretru.microservices.oauth2.server.memory.OAuth2UniqueTokenMemory;
import com.kuretru.microservices.oauth2.server.property.OAuth2ServerProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.io.Serializable;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
public class OAuth2UniqueTokenMemoryImpl implements OAuth2UniqueTokenMemory {

    private static final String REDIS_ROOT_KEY = "OAuth2UniqueTokenMemory.";
    private static final String REDIS_TOKEN_KEY = REDIS_ROOT_KEY + "token.";

    private final OAuth2ServerProperty property;
    private final RedisTemplate<String, Serializable> redisTemplate;

    @Autowired
    public OAuth2UniqueTokenMemoryImpl(OAuth2ServerProperty property, RedisTemplate<String, Serializable> redisTemplate) {
        this.property = property;
        this.redisTemplate = redisTemplate;
    }

    @Override
    public String generateAndSave(OAuth2AuthorizeDTO.Request record) {
        String token = StringUtils.randomUUID();
        while (Boolean.TRUE.equals(redisTemplate.hasKey(buildKey(token)))) {
            token = StringUtils.randomUUID();
        }
        redisTemplate.opsForValue().set(buildKey(token), record, property.getExpireTime());
        return token;
    }

    @Override
    public OAuth2AuthorizeDTO.Request getAndDelete(String token) {
        String key = buildKey(token);
        if (Boolean.FALSE.equals(redisTemplate.hasKey(key))) {
            return null;
        }
        return (OAuth2AuthorizeDTO.Request)redisTemplate.opsForValue().getAndDelete(key);
    }

    private String buildKey(String key) {
        return REDIS_TOKEN_KEY + key;
    }

}
