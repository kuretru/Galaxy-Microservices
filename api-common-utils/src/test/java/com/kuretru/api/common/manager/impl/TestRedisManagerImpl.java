package com.kuretru.api.common.manager.impl;

import com.kuretru.api.common.configuration.GeneralConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Component;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Component
public class TestRedisManagerImpl extends BaseRedisManagerImpl {

    @Autowired
    public TestRedisManagerImpl(StringRedisTemplate redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @Override
    protected String getKeyName(String rawName) {
        return "test" + GeneralConstants.ACCESS_TOKEN_KEY + rawName;
    }

}
