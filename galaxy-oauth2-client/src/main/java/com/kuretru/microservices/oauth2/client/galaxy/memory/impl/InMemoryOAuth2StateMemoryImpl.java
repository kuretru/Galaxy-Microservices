package com.kuretru.microservices.oauth2.client.galaxy.memory.impl;

import com.kuretru.microservices.oauth2.client.galaxy.GalaxyClientProperty;
import com.kuretru.microservices.oauth2.client.galaxy.memory.OAuth2StateMemory;
import org.apache.commons.lang3.NotImplementedException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
@ConditionalOnMissingBean(StringRedisTemplate.class)
public class InMemoryOAuth2StateMemoryImpl implements OAuth2StateMemory {

    private final GalaxyClientProperty property;

    @Autowired
    public InMemoryOAuth2StateMemoryImpl(GalaxyClientProperty property) {
        this.property = property;
    }

    @Override
    public String generateAndSave(String scope) {
        throw new NotImplementedException();
    }

    @Override
    public String getAndDelete(String state) {
        throw new NotImplementedException();
    }

}
