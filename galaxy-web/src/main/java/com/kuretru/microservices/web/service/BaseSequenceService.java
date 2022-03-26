package com.kuretru.microservices.web.service;

import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.exception.ServiceException;

import java.util.List;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseSequenceService<T extends BaseDTO, Q> extends BaseService<T, Q> {

    /**
     * 查询数据库中当前最大的排序号
     *
     * @return 最大的排序号
     */
    int getMaxSequence();

    /**
     * 根据UUID列表重新排序sequence字段
     *
     * @param uuidList UUID列表
     * @throws ServiceException 找不到记录时，会引发NotFound异常
     */
    void reorder(List<UUID> uuidList) throws ServiceException;

}
