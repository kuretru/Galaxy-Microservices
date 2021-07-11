package com.kuretru.api.common.controller;

import com.kuretru.api.common.constant.code.UserErrorCodes;
import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.service.BaseSequenceService;
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
    public ApiResponse<String> reorder(@RequestBody List<UUID> uuidList) throws ServiceException {
        if (uuidList == null || uuidList.isEmpty()) {
            throw new ServiceException.BadRequest(UserErrorCodes.REQUEST_PARAMETER_ERROR, "未指定ID列表");
        }
        service.reorder(uuidList);
        return ApiResponse.success("重新排序成功");
    }

}
