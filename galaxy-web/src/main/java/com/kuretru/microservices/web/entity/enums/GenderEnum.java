package com.kuretru.microservices.web.entity.enums;

import com.kuretru.microservices.common.entity.enums.BaseEnum;
import lombok.Getter;

/**
 * 性别枚举
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Getter
public enum GenderEnum implements BaseEnum<GenderEnum> {

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
