package com.kuretru.api.common.constant;

import lombok.Getter;

/**
 * 内部服务错误响应码枚举
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Getter
public enum ServiceErrorCodes implements ResponseCodes {

    /** B0001-系统执行出错 */
    SYSTEM_EXECUTION_ERROR(50001, "系统执行出错");

    private final int code;
    private final String message;

    ServiceErrorCodes(int code, String message) {
        this.code = code;
        this.message = message;
    }

}
