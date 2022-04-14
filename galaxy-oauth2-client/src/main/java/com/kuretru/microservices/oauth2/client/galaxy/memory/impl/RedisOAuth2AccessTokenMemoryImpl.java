package com.kuretru.microservices.oauth2.client.galaxy.memory.impl;

import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.oauth2.client.galaxy.entity.GalaxyAccessTokenDO;
import com.kuretru.microservices.oauth2.client.galaxy.memory.OAuth2AccessTokenMemory;
import com.kuretru.microservices.oauth2.common.constant.OAuth2Constants;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AccessTokenDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.time.Duration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
@ConditionalOnBean(StringRedisTemplate.class)
public class RedisOAuth2AccessTokenMemoryImpl implements OAuth2AccessTokenMemory {

    private static final String REDIS_ROOT_KEY = "OAuth2AccessTokenMemory.";
    private static final String REDIS_ACCESS_TOKEN_KEY = REDIS_ROOT_KEY + "accessToken.";

    private final RedisTemplate<String, Serializable> redisTemplate;

    @Autowired
    public RedisOAuth2AccessTokenMemoryImpl(RedisTemplate<String, Serializable> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @Override
    public void save(String id, OAuth2AccessTokenDTO.Response record) {
        Duration expireTime = Duration.ofMillis(record.getExpiresIn() - System.currentTimeMillis());
        GalaxyAccessTokenDO accessTokenDO = new GalaxyAccessTokenDO(
                record.getAccessToken(),
                record.getRefreshToken(),
                StringUtils.stringToSet(record.getScope(), OAuth2Constants.SCOPES_SEPARATOR)
        );
        redisTemplate.opsForValue().set(buildKey(id), accessTokenDO, expireTime);
    }

    @Override
    public GalaxyAccessTokenDO get(String id) {
        return (GalaxyAccessTokenDO)redisTemplate.opsForValue().get(buildKey(id));
    }

    @Override
    public void delete(String id) {
        redisTemplate.delete(buildKey(id));
    }

    private String buildKey(String key) {
        return REDIS_ACCESS_TOKEN_KEY + key;
    }

}
