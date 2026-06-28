package com.kuretru.microservices.common.utils;

import com.kuretru.microservices.common.entity.enums.BaseEnum;
import com.kuretru.microservices.common.entity.enums.EnumDTO;
import lombok.Getter;
import lombok.ToString;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
class EnumUtilsTest {

    @Test
    void ValueOf() {
        assertEquals(GenderEnum.UNKNOWN, EnumUtils.valueOf(GenderEnum.class, "unknown"));
        assertEquals(GenderEnum.MALE, EnumUtils.valueOf(GenderEnum.class, "male"));
        assertEquals(GenderEnum.FEMALE, EnumUtils.valueOf(GenderEnum.class, "female"));
        assertThrows(IllegalArgumentException.class, () -> EnumUtils.valueOf(GenderEnum.class, "测试"));
    }

    @Test
    void buildEnumMap() {
        Map<GenderEnum, String> map = new HashMap<>(16);
        map.put(GenderEnum.UNKNOWN, GenderEnum.UNKNOWN.getValue());
        map.put(GenderEnum.MALE, GenderEnum.MALE.getValue());
        map.put(GenderEnum.FEMALE, GenderEnum.FEMALE.getValue());
        assertEquals(map, EnumUtils.buildEnumMap(GenderEnum.values()));
    }

    @Test
    void buildStringEnumMap() {
        Map<String, String> map = new HashMap<>(16);
        map.put("UNKNOWN", GenderEnum.UNKNOWN.getValue());
        map.put("MALE", GenderEnum.MALE.getValue());
        map.put("FEMALE", GenderEnum.FEMALE.getValue());
        assertEquals(map, EnumUtils.buildStringEnumMap(GenderEnum.values()));
    }

    @Test
    void buildDTO() {
        List<EnumDTO<String>> map = new ArrayList<>();
        map.add(new EnumDTO<>("unknown", "未知"));
        map.add(new EnumDTO<>("male", "男"));
        map.add(new EnumDTO<>("female", "女"));
        assertEquals(map, EnumUtils.buildDTO(GenderEnum.values()));
    }

    @Getter
    @ToString
    private enum GenderEnum implements BaseEnum<GenderEnum> {

        /** 未知 */
        UNKNOWN("unknown", "未知"),
        /** 男 */
        MALE("male", "男"),
        /** 女 */
        FEMALE("female", "女");

        private final String value;
        private final String label;


        GenderEnum(String value, String label) {
            this.value = value;
            this.label = label;
        }

    }

}
