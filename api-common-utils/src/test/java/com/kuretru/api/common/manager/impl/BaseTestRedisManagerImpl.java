package com.kuretru.api.common.manager.impl;

import com.kuretru.api.common.ApplicationTests;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class BaseTestRedisManagerImpl extends ApplicationTests {

    private static final String TEST_KEY = "Xandria";

    @Autowired
    private TestRedisManagerImpl redisManager;

    @Test
    public void all() {
        redisManager.remove(TEST_KEY);
        assertNull(redisManager.get(TEST_KEY));
        redisManager.create(TEST_KEY, "test123");
        assertEquals("test123", redisManager.get(TEST_KEY));
        redisManager.remove(TEST_KEY);
        assertNull(redisManager.get(TEST_KEY));
    }

}