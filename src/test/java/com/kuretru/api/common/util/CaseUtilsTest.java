package com.kuretru.api.common.util;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
class CaseUtilsTest {

    @Test
    void snakeToCamel() {
        assertNull(CaseUtils.snakeToCamel(null));
        assertEquals("", CaseUtils.snakeToCamel(""));
        assertEquals(" ", CaseUtils.snakeToCamel(" "));
        assertEquals("snakeCaseToCamelCase", CaseUtils.snakeToCamel("snake_case_to_camel_case"));
    }

}
