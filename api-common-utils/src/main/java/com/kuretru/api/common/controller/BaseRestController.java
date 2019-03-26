package com.kuretru.api.common.controller;

import com.kuretru.api.common.annotation.RequestAuthorization;
import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.exception.ApiException;
import com.kuretru.api.common.exception.NotFoundException;
import com.kuretru.api.common.service.BaseService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public abstract class BaseRestController<S extends BaseService<?, ?, T>, T> extends BaseController {

    protected S service;

    /**
     * 注入对应serice实例
     *
     * @param service service实例
     */
    public abstract void setService(S service);

    @RequestAuthorization
    @GetMapping("/{id}")
    public ApiResponse get(@PathVariable("id") Long id) throws ApiException {
        T result = service.get(id);
        if (result == null) {
            throw new NotFoundException("未找到相关对象！");
        }
        return ApiResponse.success(result);
    }

    @RequestAuthorization
    @GetMapping
    public ApiResponse list() throws ApiException {
        List<T> result = service.list();
        if (result.isEmpty()) {
            throw new NotFoundException("未找到相关对象！");
        }
        return ApiResponse.success(result);
    }

    @RequestAuthorization
    @PostMapping
    public ApiResponse create(@RequestBody T record) throws ApiException {
        T result = service.save(record);
        return ApiResponse.success(result);
    }

}
