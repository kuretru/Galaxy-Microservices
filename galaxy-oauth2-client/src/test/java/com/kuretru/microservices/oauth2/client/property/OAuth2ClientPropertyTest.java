package com.kuretru.microservices.oauth2.client.property;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@SpringBootTest
class OAuth2ClientPropertyTest {

    private final OAuth2ClientProperty property;

    @Autowired
    public OAuth2ClientPropertyTest(OAuth2ClientProperty property) {
        this.property = property;
    }

    @Test
    void getGemini() {
        assertEquals("id-888", property.getGemini().getApplicationId());
        assertEquals("secret-666", property.getGemini().getApplicationSecret());
        assertEquals("http://127.0.0.1:7051", property.getGemini().getServerUrl());
        assertEquals(Duration.ofMinutes(15), property.getGemini().getStateExpireTime());
    }

}
