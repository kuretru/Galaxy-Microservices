package com.kuretru.api.common.entity.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Getter
public enum UserRoleEnum implements BaseEnum {
    /**
     * 用户
     */
    USER(0, "用户"),
    /**
     * 管理员
     */
    ADMIN(80, "管理员");


    private int code;
    @JsonValue
    private String message;

    UserRoleEnum(int code, String message) {
        this.code = code;
        this.message = message;
    }

    public static UserRoleEnum valueOf(Integer code) {
        if (code == null) {
            return null;
        }
        for (UserRoleEnum value : UserRoleEnum.values()) {
            if (value.code == code) {
                return value;
            }
        }
        return null;
    }

}
