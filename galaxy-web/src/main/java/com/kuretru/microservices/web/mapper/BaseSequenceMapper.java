package com.kuretru.microservices.web.mapper;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import com.kuretru.microservices.web.entity.data.BaseSequenceDO;
import org.apache.ibatis.annotations.Param;

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
    Integer getMaxSequence(@Param(Constants.WRAPPER) QueryWrapper<D> queryWrapper);

    /**
     * 根据UUID批量更新排序号
     *
     * @param records 要更新的数据
     * @return 受影响的行数
     */
    Integer updateSequenceByUuids(List<D> records);

}
