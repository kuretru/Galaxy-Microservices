package com.kuretru.microservices.web.controller;

import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.service.BaseSequenceService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.List;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class BaseSequenceRestController<S extends BaseSequenceService<T, Q>, T extends BaseDTO, Q> extends BaseRestController<S, T, Q> {

    public BaseSequenceRestController(S service) {
        super(service);
    }

    @PutMapping("/reorder")
    @Operation(summary = "重新排序记录")
    @Parameter(name = "uuidList", description = "新顺序的记录ID列表")
    public ApiResponse<String> reorder(@RequestBody List<UUID> uuidList) throws ServiceException {
        if (uuidList == null || uuidList.isEmpty()) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID列表");
        }
        service.reorder(uuidList);
        return ApiResponse.success("重新排序成功");
    }

}
