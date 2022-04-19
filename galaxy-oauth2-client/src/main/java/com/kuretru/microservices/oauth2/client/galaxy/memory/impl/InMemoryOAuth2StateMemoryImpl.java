package com.kuretru.microservices.oauth2.client.galaxy.memory.impl;

import com.kuretru.microservices.common.storage.InMemoryCounterStorage;
import com.kuretru.microservices.common.storage.InMemoryStorage;
import com.kuretru.microservices.oauth2.client.galaxy.GalaxyClientProperty;
import com.kuretru.microservices.oauth2.client.galaxy.memory.OAuth2StateMemory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
@ConditionalOnMissingBean(StringRedisTemplate.class)
public class InMemoryOAuth2StateMemoryImpl implements OAuth2StateMemory {

    private static final int PURGE_INTERVAL = 128;
    private final GalaxyClientProperty property;
    private final InMemoryStorage<String, String> database;

    @Autowired
    public InMemoryOAuth2StateMemoryImpl(GalaxyClientProperty property) {
        this.property = property;
        this.database = new InMemoryCounterStorage<>(PURGE_INTERVAL);
    }

    @Override
    public String generateAndSave(String redirectUri) {
        String state = UUID.randomUUID().toString();
        while (database.hasKey(state)) {
            state = UUID.randomUUID().toString();
        }
        database.set(state, redirectUri, property.getStateExpireTime());
        return state;
    }

    @Override
    public String getAndDelete(String state) {
        if (!database.hasKey(state)) {
            return null;
        }
        return database.getAndDelete(state);
    }

}
