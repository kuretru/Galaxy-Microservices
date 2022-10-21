package com.kuretru.microservices.common.utils;

import com.kuretru.microservices.common.constant.EmptyConstants;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
class UuidUtilsTest {

    @Test
    void isEmpty() {
        assertTrue(UuidUtils.isEmpty(null));
        assertTrue(UuidUtils.isEmpty(EmptyConstants.EMPTY_UUID));
        assertFalse(UuidUtils.isEmpty(UUID.randomUUID()));
    }

    @Test
    void isNotEmpty() {
        assertFalse(UuidUtils.isNotEmpty(null));
        assertFalse(UuidUtils.isNotEmpty(EmptyConstants.EMPTY_UUID));
        assertTrue(UuidUtils.isNotEmpty(UUID.randomUUID()));
    }

}
