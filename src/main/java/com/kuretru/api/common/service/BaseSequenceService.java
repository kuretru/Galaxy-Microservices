package com.kuretru.api.common.service;

import com.kuretru.api.common.entity.transfer.BaseDTO;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseSequenceService<T extends BaseDTO> extends BaseService<T> {

    /**
     * 查询数据库中当前最大的排序号
     *
     * @return 最大的排序号
     */
    int getMaxSequence();

}
