package com.kuretru.microservices.web.constant.code;

import org.springframework.http.HttpStatus;

/**
 * 业务响应码枚举接口
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface ResponseCodes {

    /**
     * 返回业务响应码
     *
     * @return 业务响应码
     */
    int getCode();

    /**
     * 返回业务信息
     *
     * @return 业务信息
     */
    String getMessage();

    /**
     * 发送该错误时应该返回的HTTP状态码
     *
     * @return HTTP状态码
     */
    HttpStatus getHttpStatus();

}
