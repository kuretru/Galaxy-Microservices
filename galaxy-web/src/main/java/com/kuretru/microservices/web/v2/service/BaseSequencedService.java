package com.kuretru.microservices.web.v2.service;

import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;

import java.util.List;
import java.util.UUID;

public interface BaseSequencedService<T extends BaseDTO, Q> extends BaseService<T, Q> {

    /**
     * 查询数据库中当前最大的排序号
     *
     * @param record DTO
     * @return 最大的排序号
     */
    int getMaxSequence(T record);

    /**
     * 根据UUID列表重新排序sequence字段
     *
     * @param uuidList UUID列表
     * @throws ServiceException 找不到记录时，会引发NotFound异常
     */
    void reorder(List<UUID> uuidList) throws ServiceException;

}
