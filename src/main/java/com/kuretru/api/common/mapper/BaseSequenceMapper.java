package com.kuretru.api.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.api.common.entity.data.BaseSequenceDO;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseSequenceMapper<D extends BaseSequenceDO> extends BaseMapper<D> {

    /**
     * 查询数据库中当前最大的排序号
     *
     * @return 最大的排序号
     */
    Integer getMaxSequence();

}
