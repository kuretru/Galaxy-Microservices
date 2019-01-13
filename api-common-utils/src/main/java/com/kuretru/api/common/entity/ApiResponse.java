package com.kuretru.api.common.entity;

import lombok.Data;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
public class ApiResponse {

    private Integer code;
    private String message;
    private Object data;

    public ApiResponse() {
        super();
    }

    public ApiResponse(Integer code, String message, Object data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    public static ApiResponse success(Object data) {
        return new ApiResponse(2000, "success", data);
    }

    public static ApiResponse created(Object data) {
        return new ApiResponse(2001, "created", data);
    }

    public static ApiResponse updated(Object data) {
        return new ApiResponse(2002, "updated", data);
    }

    public static ApiResponse deleted(Object data) {
        return new ApiResponse(2003, "deleted", data);
    }

}
