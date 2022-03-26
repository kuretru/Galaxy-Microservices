package com.kuretru.microservices.web.constant.code;

import lombok.Getter;

/**
 * 业务正常执行的响应码枚举，范围在100~199之间
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Getter
public enum SuccessCodes implements ResponseCodes {

    /** 成功 */
    SUCCESS(100, "成功"),
    /** 已创建 */
    CREATED(101, "已创建"),
    /** 已更新 */
    UPDATED(102, "已更新"),
    /** 已删除 */
    REMOVED(103, "已删除"),
    /** 资源不存在 */
    NOT_FOUND(110, "资源不存在");

    private final int code;
    private final String message;

    SuccessCodes(int code, String message) {
        this.code = code;
        this.message = message;
    }

}
