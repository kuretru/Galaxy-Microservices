package com.kuretru.microservices.web.v2.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.microservices.web.entity.interfaces.Sequenced;
import com.kuretru.microservices.web.v2.entity.data.BaseDO;

public interface BaseSequencedMapper<D extends BaseDO & Sequenced> extends BaseMapper<D> {
}
