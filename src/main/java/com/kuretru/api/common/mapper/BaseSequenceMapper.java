package com.kuretru.api.common.mapper;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.api.common.entity.data.BaseSequenceDO;

import java.util.List;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseSequenceMapper<D extends BaseSequenceDO> extends BaseMapper<D> {

    /**
     * 查询数据库中当前最大的排序号
     *
     * @param queryWrapper 查询条件
     * @return 最大的排序号
     */
    Integer getMaxSequence(QueryWrapper<D> queryWrapper);

    /**
     * 根据UUID批量更新排序号
     *
     * @param records 要更新的数据
     * @return 受影响的行数
     */
    Integer updateSequenceByUuids(List<D> records);

}
