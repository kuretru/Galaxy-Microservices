package com.kuretru.microservices.web.v2.controller;

import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.entity.PaginationQuery;
import com.kuretru.microservices.web.entity.PaginationResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.v2.service.BaseService;
import com.kuretru.microservices.web.v2.service.impl.BaseServiceImpl;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springdoc.core.annotations.ParameterObject;
import org.springframework.core.ResolvableType;
import org.springframework.http.HttpStatus;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 提供基本RESTful API功能的控制器，在派生类上设置@RequestMapping注解即可直接暴露出服务
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseRestController<S extends BaseService<T, Q>, T extends BaseDTO, Q> extends BaseCrudController<S, T, Q> {

    public BaseRestController(S service) {
        super(service);
        ResolvableType type = ResolvableType.forClass(getClass()).as(BaseServiceImpl.class);
    }

    @GetMapping("/{id}")
    @Operation(summary = "根据ID查询记录")
    @Override
    public ApiResponse<T> get(@Parameter(description = "记录ID") @PathVariable Long id) throws ServiceException {
        if (id == null || id == 0) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        }
        return super.get(id);
    }

    @GetMapping
    @Operation(summary = "根据分页参数和查询条件查询记录列表")
    public ApiResponse<PaginationResponse<T>> list(@ParameterObject @Validated PaginationQuery paginationQuery, @ParameterObject @Validated Q query) throws ServiceException {
        return super.listByPage(paginationQuery, query);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    @Operation(summary = "创建新记录")
    @Override
    public ApiResponse<T> create(@Parameter(description = "记录内容", required = true) @Validated @RequestBody T record) throws ServiceException {
        if (record == null) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定记录");
        }
        return super.create(record);
    }

    @PutMapping("/{id}")
    @Operation(summary = "更新记录")
    @Override
    public ApiResponse<T> update(@Parameter(description = "记录ID") @PathVariable Long id, @Parameter(description = "记录内容", required = true) @Validated @RequestBody T record) throws ServiceException {
        if (id == null || id == 0) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        } else if (record == null) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定记录");
        } else if (!id.equals(record.getId())) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定ID与记录ID不符");
        }
        return super.update(id, record);
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "根据ID删除记录")
    @Override
    public ApiResponse<String> remove(@Parameter(description = "记录ID") @PathVariable Long id) throws ServiceException {
        if (id == null || id == 0) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID或ID错误");
        }
        return super.remove(id);
    }

}
