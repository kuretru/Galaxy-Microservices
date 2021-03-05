package com.kuretru.api.common.controller;

import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.service.BaseService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.UUID;

/**
 * 提供基本RESTful API功能的控制器，在派生类上设置@RequestMapping注解即可直接暴露出服务
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseRestController<S extends BaseService<T>, T extends BaseDTO> extends BaseCrudController<S, T> {

    protected BaseRestController(S service) {
        super(service);
    }

    @GetMapping("/{id}")
    @Override
    public ApiResponse<T> get(@PathVariable("id") UUID id) throws ServiceException {
        return super.get(id);
    }

    @GetMapping
    @Override
    public ApiResponse<List<T>> list() throws ServiceException {
        return super.list();
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    @Override
    public ApiResponse<T> create(T record) throws ServiceException {
        return super.create(record);
    }

    @PutMapping("/{id}")
    @Override
    public ApiResponse<T> update(@PathVariable("id") UUID id, T record) throws ServiceException {
        return super.update(id, record);
    }

    @DeleteMapping("/{id}")
    @Override
    public ApiResponse<String> remove(@PathVariable("id") UUID id) throws ServiceException {
        return super.remove(id);
    }

}
