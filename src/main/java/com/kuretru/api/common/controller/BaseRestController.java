package com.kuretru.api.common.controller;

import com.kuretru.api.common.constant.EmptyConstants;
import com.kuretru.api.common.constant.code.UserErrorCodes;
import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.entity.PaginationQuery;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.service.BaseService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
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
    @Operation(summary = "根据ID查询记录")
    @Parameter(name = "id", description = "记录ID")
    @Override
    public ApiResponse<T> get(@PathVariable("id") UUID id) throws ServiceException {
        if (id == null || EmptyConstants.EMPTY_UUID.equals(id)) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        }
        return super.get(id);
    }

    @GetMapping
    @Operation(summary = "根据分页参数和查询条件查询记录列表", description = "若分页参数存在，则返回分页查询结果")
    @Parameter(name = "paginationQuery", description = "分页参数", example = "{current=1&page_size=10}")
    @Parameter(name = "query", description = "查询条件")
    public ApiResponse<?> list(PaginationQuery paginationQuery, Q query) throws ServiceException {
        if (paginationQuery != null && paginationQuery.getCurrent() != null && paginationQuery.getPageSize() != null) {
            return super.listByPage(paginationQuery, query);
        }
        return super.list(query);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    @Operation(summary = "创建新记录")
    @Parameter(name = "record", description = "记录内容", required = true)
    @Override
    public ApiResponse<T> create(@RequestBody T record) throws ServiceException {
        if (record == null) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定记录");
        }
        return super.create(record);
    }

    @PutMapping("/{id}")
    @Operation(summary = "更新记录")
    @Parameter(name = "id", description = "记录ID")
    @Parameter(name = "record", description = "记录内容", required = true)
    @Override
    public ApiResponse<T> update(@PathVariable("id") UUID id, @RequestBody T record) throws ServiceException {
        if (id == null || EmptyConstants.EMPTY_UUID.equals(id)) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        }
        if (record == null) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定记录");
        }
        return super.update(id, record);
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "根据ID删除记录")
    @Parameter(name = "id", description = "记录ID")
    @Override
    public ApiResponse<String> remove(@PathVariable("id") UUID id) throws ServiceException {
        if (id == null || EmptyConstants.EMPTY_UUID.equals(id)) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        }
        return super.remove(id);
    }

}
