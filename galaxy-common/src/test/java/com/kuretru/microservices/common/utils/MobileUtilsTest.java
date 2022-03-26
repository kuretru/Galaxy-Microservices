package com.kuretru.microservices.common.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class MobileUtilsTest {

    @Test
    void blurMobile() {
        String mobile = "18612345678";
        assertEquals("186****5678", MobileUtils.blurMobile(mobile));
    }

}
