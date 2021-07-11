package com.kuretru.api.common.controller;

import com.kuretru.api.common.constant.UuidConstants;
import com.kuretru.api.common.constant.code.UserErrorCodes;
import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.entity.PaginationQuery;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.service.BaseService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

/**
 * 提供基本RESTful API功能的控制器，在派生类上设置@RequestMapping注解即可直接暴露出服务
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseRestController<S extends BaseService<T, Q>, T extends BaseDTO, Q> extends BaseCrudController<S, T, Q> {

    public BaseRestController(S service) {
        super(service);
    }

    @GetMapping("/{id}")
    @Override
    public ApiResponse<T> get(@PathVariable("id") UUID id) throws ServiceException {
        if (id == null || UuidConstants.EMPTY.equals(id)) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        }
        return super.get(id);
    }

    @GetMapping
    public ApiResponse<?> list(PaginationQuery paginationQuery, Q query) throws ServiceException {
        if (paginationQuery != null && paginationQuery.getCurrent() != null && paginationQuery.getPageSize() != null) {
            return super.listByPage(paginationQuery, query);
        }
        return super.list(query);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    @Override
    public ApiResponse<T> create(@RequestBody T record) throws ServiceException {
        if (record == null) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定记录");
        }
        return super.create(record);
    }

    @PutMapping("/{id}")
    @Override
    public ApiResponse<T> update(@PathVariable("id") UUID id, @RequestBody T record) throws ServiceException {
        if (id == null || UuidConstants.EMPTY.equals(id)) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        }
        if (record == null) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定记录");
        }
        return super.update(id, record);
    }

    @DeleteMapping("/{id}")
    @Override
    public ApiResponse<String> remove(@PathVariable("id") UUID id) throws ServiceException {
        if (id == null || UuidConstants.EMPTY.equals(id)) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        }
        return super.remove(id);
    }

}
