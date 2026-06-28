package com.kuretru.microservices.web.v2.controller;

import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.v2.service.BaseService;
import com.kuretru.microservices.web.v2.service.SequencedService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;

public abstract class BaseSequencedRestController<S extends BaseService<T, Q> & SequencedService<T>, T extends BaseDTO, Q> extends BaseRestController<S, T, Q> {

    public BaseSequencedRestController(S service) {
        super(service);
    }

    @PutMapping("/reorder")
    @Operation(summary = "重新排序记录")
    @Parameter(name = "idList", description = "新顺序的记录ID列表")
    public ApiResponse<String> reorder(@RequestBody List<Long> idList) throws ServiceException {
        if (idList == null || idList.isEmpty()) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID列表");
        }
        service.reorder(idList);
        return ApiResponse.success("重新排序成功");
    }

}
