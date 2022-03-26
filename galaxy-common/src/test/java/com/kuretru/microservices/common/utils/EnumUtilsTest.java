package com.kuretru.microservices.common.utils;

import com.kuretru.microservices.common.entity.enums.BaseEnum;
import lombok.Getter;
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

    @Getter
    private enum GenderEnum implements BaseEnum<GenderEnum> {

        /** 未知 */
        UNKNOWN((short)0, "未知"),
        /** 男 */
        MALE((short)1, "男"),
        /** 女 */
        FEMALE((short)2, "女");

        /** 枚举编号 */
        private final short code;

        /** 枚举内容 */
        private final String value;

        GenderEnum(short code, String value) {
            this.code = code;
            this.value = value;
        }

    }

}
