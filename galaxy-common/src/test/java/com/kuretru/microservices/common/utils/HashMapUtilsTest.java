package com.kuretru.microservices.common.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class HashMapUtilsTest {

    @Test
    void initialCapacity() {
        assertEquals(22, HashMapUtils.initialCapacity(16));
        assertEquals(33, HashMapUtils.initialCapacity(16, 0.5));
    }

}
