package com.kuretru.api.common.constant;

import lombok.Getter;

/**
 * 正常响应码枚举
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Getter
public enum SuccessCodes implements ResponseCodes {

    /** 成功 */
    SUCCESS(100, "成功"),
    /** 未找到该资源 */
    NOT_FOUND(110, "未找到该资源");

    private final int code;
    private final String message;

    SuccessCodes(int code, String message) {
        this.code = code;
        this.message = message;
    }

}
