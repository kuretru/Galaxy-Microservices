package com.kuretru.api.common.constant.code;

import lombok.Getter;

/**
 * 错误发生在用户端的响应码枚举，范围在10000~19999之间
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Getter
public enum UserErrorCodes implements ResponseCodes {

    /** A0200-用户密码错误 */
    WRONG_USER_PASSWORD(10200, "用户密码错误"),

    /** A0300-访问权限异常 */
    ACCESS_PERMISSION_ERROR(10300, "访问权限异常"),
    /** A0340-用户签名异常 */
    WRONG_USER_SIGNATURE(10340, "用户签名异常"),

    /** A0400-用户请求参数错误 */
    REQUEST_PARAMETER_ERROR(10400, "用户请求参数错误"),
    /** A0410-请求必填参数为空 */
    MISSING_REQUIRED_PARAMETERS(10410, "请求必填参数为空"),
    /** A0412-订购数量为空 */
    ORDER_QUANTITY_EMPTY(10412, "订购数量为空"),

    /** A0506-用户重复请求 */
    USER_REPEATED_REQUEST(10506, "用户重复请求");

    private final int code;
    private final String message;

    UserErrorCodes(int code, String message) {
        this.code = code;
        this.message = message;
    }

}
