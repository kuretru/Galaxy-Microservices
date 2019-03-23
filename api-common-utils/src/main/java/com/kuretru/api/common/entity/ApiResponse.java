package com.kuretru.api.common.entity;

import lombok.Data;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
public class ApiResponse<D> {

    public static final Integer SUCCESS = 2000;
    public static final Integer CREATED = 2001;
    public static final Integer UPDATED = 2002;
    public static final Integer DELETED = 2003;

    private Integer code;
    private String message;
    private D data;

    public ApiResponse() {
        super();
    }

    public ApiResponse(Integer code, String message, D data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    public static ApiResponse success(Object data) {
        return new ApiResponse<>(SUCCESS, "success", data);
    }

    public static ApiResponse created(Object data) {
        return new ApiResponse<>(CREATED, "created", data);
    }

    public static ApiResponse updated(Object data) {
        return new ApiResponse<>(UPDATED, "updated", data);
    }

    public static ApiResponse deleted(Object data) {
        return new ApiResponse<>(DELETED, "deleted", data);
    }

}
