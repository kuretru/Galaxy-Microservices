package com.kuretru.microservices.web.v2.service;

import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;

import java.util.List;

public interface SequencedService<T extends BaseDTO> {

    /**
     * 查询数据库中当前最大的排序号
     *
     * @param record DTO
     * @return 最大的排序号，没有返回0
     */
    int getMaxSequence(T record);

    /**
     * 根据ID列表重新排序sequence字段
     *
     * @param idList ID列表
     * @throws ServiceException 找不到记录时，会引发NotFound异常
     */
    void reorder(List<Long> idList) throws ServiceException;

}
