package com.kuretru.microservices.oauth2.client.galaxy;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@SpringBootTest
class GalaxyClientPropertyTest {

    private final GalaxyClientProperty property;

    @Autowired
    public GalaxyClientPropertyTest(GalaxyClientProperty property) {
        this.property = property;
    }

    @Test
    void getGemini() {
        assertEquals("id-888", property.getApplicationId());
        assertEquals("secret-666", property.getApplicationSecret());
        assertEquals("http://127.0.0.1:7051", property.getServerUrl());
        assertEquals(Duration.ofMinutes(15), property.getStateExpireTime());
    }

}
