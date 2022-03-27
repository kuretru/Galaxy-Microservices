package com.kuretru.microservices.oauth2.client.property;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

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
    void getApplicationId() {
        assertEquals("id-888", property.getApplicationId());
    }

    @Test
    void getApplicationSecret() {
        assertEquals("secret-666", property.getApplicationSecret());
    }

}
