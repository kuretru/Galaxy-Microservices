package com.kuretru.microservices.oauth2.client.memory.impl;

import com.kuretru.microservices.oauth2.client.memory.OAuth2StateMemory;
import com.kuretru.microservices.oauth2.client.property.OAuth2ClientProperty;
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

    private final OAuth2ClientProperty property;

    @Autowired
    public InMemoryOAuth2StateMemoryImpl(OAuth2ClientProperty property) {
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
