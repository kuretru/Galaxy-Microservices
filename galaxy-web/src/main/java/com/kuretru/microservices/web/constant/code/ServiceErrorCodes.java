package com.kuretru.microservices.web.constant.code;

import lombok.Getter;
import org.springframework.http.HttpStatus;

/**
 * 错误发生在内部服务的响应码枚举，范围在50000~59999之间
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Getter
public enum ServiceErrorCodes implements ResponseCodes {

    /** B0001-系统执行出错 */
    SYSTEM_EXECUTION_ERROR(50001, "系统执行出错", HttpStatus.INTERNAL_SERVER_ERROR),
    SYSTEM_NOT_IMPLEMENTED(50099, "系统尚未实现", HttpStatus.INTERNAL_SERVER_ERROR);

    private final int code;
    private final String message;
    private final HttpStatus httpStatus;

    ServiceErrorCodes(int code, String message, HttpStatus httpStatus) {
        this.code = code;
        this.message = message;
        this.httpStatus = httpStatus;
    }

}
