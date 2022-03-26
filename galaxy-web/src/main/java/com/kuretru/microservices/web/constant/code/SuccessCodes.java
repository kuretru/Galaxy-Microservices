package com.kuretru.microservices.web.constant.code;

import lombok.Getter;
import org.springframework.http.HttpStatus;

/**
 * 业务正常执行的响应码枚举，范围在100~199之间
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Getter
public enum SuccessCodes implements ResponseCodes {

    /** 成功 */
    SUCCESS(100, "成功", HttpStatus.OK),
    /** 已创建 */
    CREATED(101, "已创建", HttpStatus.CREATED),
    /** 已更新 */
    UPDATED(102, "已更新", HttpStatus.OK),
    /** 已删除 */
    REMOVED(103, "已删除", HttpStatus.OK),
    /** 资源不存在 */
    NOT_FOUND(110, "资源不存在", HttpStatus.OK);

    private final int code;
    private final String message;
    private final HttpStatus httpStatus;

    SuccessCodes(int code, String message, HttpStatus httpStatus) {
        this.code = code;
        this.message = message;
        this.httpStatus = httpStatus;
    }

}
