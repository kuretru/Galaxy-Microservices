package com.kuretru.api.common.util;

import com.kuretru.api.common.entity.enums.GenderEnum;
import org.junit.jupiter.api.Test;

import java.util.Map;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
class EnumUtilsTest {

    @Test
    void valueOfCode() {
        assertEquals(GenderEnum.UNKNOWN, EnumUtils.valueOf(GenderEnum.class, (short)0));
        assertEquals(GenderEnum.MALE, EnumUtils.valueOf(GenderEnum.class, (short)1));
        assertEquals(GenderEnum.FEMALE, EnumUtils.valueOf(GenderEnum.class, (short)2));
        assertThrows(IllegalArgumentException.class, () -> EnumUtils.valueOf(GenderEnum.class, (short)-1));
    }

    @Test
    void ValueOfValue() {
        assertEquals(GenderEnum.UNKNOWN, EnumUtils.valueOf(GenderEnum.class, "未知"));
        assertEquals(GenderEnum.MALE, EnumUtils.valueOf(GenderEnum.class, "男"));
        assertEquals(GenderEnum.FEMALE, EnumUtils.valueOf(GenderEnum.class, "女"));
        assertThrows(IllegalArgumentException.class, () -> EnumUtils.valueOf(GenderEnum.class, "测试"));
    }

    @Test
    void buildEnumMap() {
        Map<GenderEnum, String> map = new TreeMap<>();
        map.put(GenderEnum.UNKNOWN, GenderEnum.UNKNOWN.getValue());
        map.put(GenderEnum.MALE, GenderEnum.MALE.getValue());
        map.put(GenderEnum.FEMALE, GenderEnum.FEMALE.getValue());
        assertEquals(map, EnumUtils.buildEnumMap(GenderEnum.values()));
    }

}
