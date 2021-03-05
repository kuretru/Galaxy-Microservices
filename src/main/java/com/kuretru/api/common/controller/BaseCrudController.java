package com.kuretru.api.common.controller;

import com.kuretru.api.common.constant.UserErrorCodes;
import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.service.BaseService;
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
public abstract class BaseCrudController<S extends BaseService<T>, T extends BaseDTO> extends BaseController {

    protected final S service;

    protected BaseCrudController(S service) {
        this.service = service;
    }

    public ApiResponse<T> get(UUID id) throws ServiceException {
        T result = service.get(id);
        if (null == result) {
            // 指定ID查询单个实体但实体不存在时，认为是用户方ID输入错误，因此抛异常
            throw new ServiceException.NotFound(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        }
        return ApiResponse.success(result);
    }

    public ApiResponse<List<T>> list() throws ServiceException {
        List<T> result = service.list();
        if (null == result) {
            result = new ArrayList<>();
        }
        if (result.isEmpty()) {
            // 批量查询实体但实体不存在时，认为实体确实有不存在的可能，因此返回相应业务状态码和空列表
            return ApiResponse.notFound(result);
        }
        return ApiResponse.success(result);
    }

    @ResponseStatus(HttpStatus.CREATED)
    public ApiResponse<T> create(T record) throws ServiceException {
        T result = service.save(record);
        return ApiResponse.created(result);
    }

    public ApiResponse<T> update(UUID id, T record) throws ServiceException {
        record.setId(id);
        T result = service.update(record);
        return ApiResponse.updated(result);
    }

    public ApiResponse<String> remove(UUID id) throws ServiceException {
        service.remove(id);
        return ApiResponse.removed("资源" + id.toString() + "已删除");
    }

}
