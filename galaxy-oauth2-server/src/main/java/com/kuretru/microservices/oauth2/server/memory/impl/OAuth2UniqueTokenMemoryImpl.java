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
        redisTemplate.opsForValue().set(REDIS_TOKEN_KEY + token, record, property.getExpireTime());
        return token;
    }

    private String buildKey(String key) {
        return REDIS_TOKEN_KEY + key;
    }

}
