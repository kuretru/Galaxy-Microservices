package com.kuretru.microservices.authentication.property;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@SpringBootTest
class AuthenticationPropertyTest {

    private final AuthenticationProperty property;

    @Autowired
    public AuthenticationPropertyTest(AuthenticationProperty property) {
        this.property = property;
    }

    @Test
    void getExpireTime() {
        assertEquals(Duration.ofHours(4), property.getExpireTime());
    }

}
