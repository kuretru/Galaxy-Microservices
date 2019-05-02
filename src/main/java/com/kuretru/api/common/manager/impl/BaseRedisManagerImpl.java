package com.kuretru.api.common.manager.impl;

import com.kuretru.api.common.configuration.GeneralConstants;
import com.kuretru.api.common.manager.RedisManager;
import org.springframework.data.redis.core.StringRedisTemplate;

import java.util.concurrent.TimeUnit;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public abstract class BaseRedisManagerImpl implements RedisManager {

    protected StringRedisTemplate redisTemplate;

    public BaseRedisManagerImpl(StringRedisTemplate redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    /**
     * 获取键的代理方法
     *
     * @param rawName 原始键名称
     * @return 正式键名称
     */
    protected abstract String getKeyName(String rawName);

    @Override
    public String get(String key) {
        return redisTemplate.opsForValue().get(
                getKeyName(key)
        );
    }

    @Override
    public void create(String key, String value) {
        redisTemplate.opsForValue().set(
                getKeyName(key),
                value,
                GeneralConstants.ACCESS_TOKEN_EXPIRES_MINUTE,
                TimeUnit.MINUTES
        );
    }

    @Override
    public void update(String key) {
        redisTemplate.expire(
                getKeyName(key),
                GeneralConstants.ACCESS_TOKEN_EXPIRES_MINUTE,
                TimeUnit.MINUTES
        );
    }

    @Override
    public void remove(String key) {
        redisTemplate.delete(
                getKeyName(key)
        );
    }

}
