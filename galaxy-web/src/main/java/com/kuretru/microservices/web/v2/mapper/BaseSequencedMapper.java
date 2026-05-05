package com.kuretru.microservices.web.v2.mapper;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import com.kuretru.microservices.web.entity.interfaces.Sequenced;
import com.kuretru.microservices.web.v2.entity.data.BaseDO;
import org.apache.ibatis.annotations.Param;

public interface BaseSequencedMapper<D extends BaseDO & Sequenced> extends BaseMapper<D> {

    /**
     * 查询数据库中当前最大的排序号
     *
     * @param queryWrapper 查询条件
     * @return 最大的排序号
     */
    Integer getMaxSequence(@Param(Constants.WRAPPER) QueryWrapper<D> queryWrapper);

}
