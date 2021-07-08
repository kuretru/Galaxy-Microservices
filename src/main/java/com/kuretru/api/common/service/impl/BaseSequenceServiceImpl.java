package com.kuretru.api.common.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.kuretru.api.common.entity.data.BaseSequenceDO;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.mapper.BaseSequenceMapper;
import com.kuretru.api.common.service.BaseSequenceService;

import java.time.Instant;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseSequenceServiceImpl<M extends BaseSequenceMapper<D>, D extends BaseSequenceDO, T extends BaseDTO, Q> extends BaseServiceImpl<M, D, T, Q> implements BaseSequenceService<T, Q> {

    protected BaseSequenceServiceImpl(M mapper, Class<D> doClass, Class<T> dtoClass) {
        super(mapper, doClass, dtoClass);
    }

    @Override
    public int getMaxSequence() {
        Integer result = mapper.getMaxSequence();
        return null == result ? 0 : result;
    }

    @Override
    public T save(T record) throws ServiceException {
        D data = dtoToDo(record);
        data.setUuid(UUID.randomUUID().toString());
        Instant now = Instant.now();
        data.setCreateTime(now);
        data.setUpdateTime(now);
        data.setSequence(getMaxSequence() + 1);
        mapper.insert(data);
        return super.get(data.getId());
    }

    @Override
    protected void addDefaultOrderBy(QueryWrapper<D> queryWrapper) {
        queryWrapper.orderByAsc("sequence");
    }

}
