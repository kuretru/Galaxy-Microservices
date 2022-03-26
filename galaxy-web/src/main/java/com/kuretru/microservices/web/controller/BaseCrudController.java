package com.kuretru.microservices.web.controller;

import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.entity.PaginationQuery;
import com.kuretru.microservices.web.entity.PaginationResponse;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.service.BaseService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * 提供基本CRUD功能的控制器，继承后可获得一些基本的参数检验功能
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseCrudController<S extends BaseService<T, Q>, T extends BaseDTO, Q> extends BaseController {

    protected final S service;

    public BaseCrudController(S service) {
        this.service = service;
    }

    protected ApiResponse<T> get(UUID id) throws ServiceException {
        T result = service.get(id);
        if (null == result) {
            // 指定ID查询单个实体但实体不存在时，认为是用户方ID输入错误，因此抛异常
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        }
        return ApiResponse.success(result);
    }

    protected ApiResponse<List<T>> list(Q query) throws ServiceException {
        List<T> result = service.list(query);
        if (null == result) {
            result = new ArrayList<>();
        }
        if (result.isEmpty()) {
            // 批量查询实体但实体不存在时，认为实体确实有不存在的可能，因此返回相应业务状态码和空列表
            return ApiResponse.notFound(result);
        }
        return ApiResponse.success(result);
    }

    protected ApiResponse<PaginationResponse<T>> listByPage(PaginationQuery paginationQuery, Q query) throws ServiceException {
        PaginationResponse<T> result = service.list(paginationQuery, query);
        if (null == result.getList()) {
            result.setList(new ArrayList<>());
        }
        if (result.getList().isEmpty()) {
            return ApiResponse.notFound(result);
        }
        return ApiResponse.success(result);
    }

    @ResponseStatus(HttpStatus.CREATED)
    protected ApiResponse<T> create(T record) throws ServiceException {
        T result = service.save(record);
        return ApiResponse.created(result);
    }

    protected ApiResponse<T> update(UUID id, T record) throws ServiceException {
        record.setId(id);
        T result = service.update(record);
        return ApiResponse.updated(result);
    }

    protected ApiResponse<String> remove(UUID id) throws ServiceException {
        service.remove(id);
        return ApiResponse.removed("资源已删除");
    }

}
