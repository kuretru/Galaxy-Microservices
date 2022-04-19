package com.kuretru.microservices.oauth2.client.galaxy.memory.impl;

import com.kuretru.microservices.common.storage.InMemoryCounterStorage;
import com.kuretru.microservices.common.storage.InMemoryStorage;
import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.oauth2.client.galaxy.entity.GalaxyAccessTokenDO;
import com.kuretru.microservices.oauth2.client.galaxy.memory.OAuth2AccessTokenMemory;
import com.kuretru.microservices.oauth2.common.constant.OAuth2Constants;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AccessTokenDTO;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.time.Duration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
@ConditionalOnMissingBean(StringRedisTemplate.class)
public class InMemoryOAuth2AccessTokenMemoryImpl implements OAuth2AccessTokenMemory {

    private static final int PURGE_INTERVAL = 128;
    private final InMemoryStorage<String, GalaxyAccessTokenDO> database;

    public InMemoryOAuth2AccessTokenMemoryImpl() {
        this.database = new InMemoryCounterStorage<>(PURGE_INTERVAL);
    }

    @Override
    public void save(String id, OAuth2AccessTokenDTO.Response record) {
        Duration expireTime = Duration.ofMillis(record.getExpiresIn() - System.currentTimeMillis());
        GalaxyAccessTokenDO accessTokenDO = new GalaxyAccessTokenDO(record.getAccessToken(), record.getRefreshToken(), StringUtils.stringToSet(record.getScope(), OAuth2Constants.SCOPES_SEPARATOR));
        database.set(id, accessTokenDO, expireTime);
    }

    @Override
    public GalaxyAccessTokenDO get(String id) {
        return database.get(id);
    }

    @Override
    public void delete(String id) {
        database.delete(id);
    }

}
