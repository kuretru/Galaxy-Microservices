package com.kuretru.api.common.manager.impl;

import com.kuretru.api.common.ApplicationTest;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@RunWith(SpringRunner.class)
@SpringBootTest
@EnableAutoConfiguration(exclude = DataSourceAutoConfiguration.class)
public class BaseTestRedisManagerImpl extends ApplicationTest {

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