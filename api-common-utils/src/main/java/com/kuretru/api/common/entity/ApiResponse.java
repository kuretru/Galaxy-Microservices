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

    public static final Integer ERROR = 4000;
    public static final Integer MISSING = 4001;
    public static final Integer INVALID = 4002;
    public static final Integer NOT_FOUND = 4004;

    public static final Integer UNAUTHORIZED = 4010;

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

    public static ApiResponse removed() {
        return removed("删除成功");
    }

    public static ApiResponse removed(Object data) {
        return new ApiResponse<>(DELETED, "removed", data);
    }

    public static ApiResponse error(Object data) {
        return new ApiResponse<>(ERROR, "error", data);
    }

    public static ApiResponse missingParameters(Object data) {
        return new ApiResponse<>(MISSING, "missing parameters", data);
    }

    public static ApiResponse invalidParameters(Object data) {
        return new ApiResponse<>(INVALID, "invalid parameters", data);
    }

    public static ApiResponse notFound(Object data) {
        return new ApiResponse<>(NOT_FOUND, "not found", data);
    }

    public static ApiResponse unauthorized(Object data) {
        return new ApiResponse<>(UNAUTHORIZED, "unauthorized", data);
    }

}
