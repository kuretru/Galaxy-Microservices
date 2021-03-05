package com.kuretru.api.common.constant;

import lombok.Getter;

/**
 * 错误发生在内部服务的响应码枚举，范围在50000~59999之间
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
