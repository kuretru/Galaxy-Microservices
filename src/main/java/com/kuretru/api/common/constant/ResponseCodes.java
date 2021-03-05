package com.kuretru.api.common.constant;

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

}
