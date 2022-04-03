package com.kuretru.microservices.oauth2.server.memory.impl;

import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.oauth2.server.entity.OAuth2ApprovedBO;
import com.kuretru.microservices.oauth2.server.memory.OAuth2UniqueCodeMemory;
import com.kuretru.microservices.oauth2.server.property.OAuth2ServerProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.io.Serializable;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
public class OAuth2UniqueCodeMemoryImpl implements OAuth2UniqueCodeMemory {

    private static final String REDIS_ROOT_KEY = "OAuth2UniqueCodeMemory.";
    private static final String REDIS_CODE_KEY = REDIS_ROOT_KEY + "code.";

    private final OAuth2ServerProperty property;
    private final RedisTemplate<String, Serializable> redisTemplate;

    @Autowired
    public OAuth2UniqueCodeMemoryImpl(OAuth2ServerProperty property, RedisTemplate<String, Serializable> redisTemplate) {
        this.property = property;
        this.redisTemplate = redisTemplate;
    }

    @Override
    public String generateAndSave(OAuth2ApprovedBO record) {
        String code = StringUtils.randomUUID();
        while (Boolean.TRUE.equals(redisTemplate.hasKey(buildKey(code)))) {
            code = StringUtils.randomUUID();
        }
        redisTemplate.opsForValue().set(buildKey(code), record, property.getExpireTime());
        return code;
    }

    @Override
    public OAuth2ApprovedBO getAndDelete(String code) {
        String key = buildKey(code);
        if (Boolean.FALSE.equals(redisTemplate.hasKey(key))) {
            return null;
        }
        return (OAuth2ApprovedBO)redisTemplate.opsForValue().getAndDelete(key);
    }

    private String buildKey(String key) {
        return REDIS_CODE_KEY + key;
    }

}
