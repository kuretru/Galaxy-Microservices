package com.kuretru.api.common.entity.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Getter
public enum WeappGenderEnum {
    /**
     * 性别未知
     */
    UNKNOWN(0, "未知"),
    /**
     * 男性
     */
    MALE(1, "男"),
    /**
     * 女性
     */
    FEMALE(2, "女");

    private int code;
    @JsonValue
    private String message;

    WeappGenderEnum(int code, String message) {
        this.code = code;
        this.message = message;
    }

    public static WeappGenderEnum valueOf(Integer code) {
        if (code == null) {
            return null;
        }
        for (WeappGenderEnum value : WeappGenderEnum.values()) {
            if (value.code == code) {
                return value;
            }
        }
        return null;
    }

}
