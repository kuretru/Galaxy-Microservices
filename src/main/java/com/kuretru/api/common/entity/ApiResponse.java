package com.kuretru.api.common.entity;

import com.kuretru.api.common.constant.ResponseCodes;
import com.kuretru.api.common.constant.SuccessCodes;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * API统一响应实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class ApiResponse<D> {

    /** 业务响应码 */
    private Integer code;
    /** 业务信息 */
    private String message;
    /** 业务数据 */
    private D data;

    public static <T> ApiResponse<T> success(T data) {
        return build(SuccessCodes.SUCCESS, data);
    }

    public static <T> ApiResponse<T> created(T data) {
        return build(SuccessCodes.CREATED, data);
    }

    public static <T> ApiResponse<T> updated(T data) {
        return build(SuccessCodes.UPDATED, data);
    }

    public static <T> ApiResponse<T> removed(T data) {
        return build(SuccessCodes.REMOVED, data);
    }

    public static <T> ApiResponse<T> notFound(T data) {
        return build(SuccessCodes.NOT_FOUND, data);
    }

    public static <T> ApiResponse<T> build(ResponseCodes code, T data) {
        return new ApiResponse<>(code.getCode(), code.getMessage(), data);
    }

}
