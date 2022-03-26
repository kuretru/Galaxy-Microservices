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
