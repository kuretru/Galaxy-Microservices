package com.kuretru.api.common.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.kuretru.api.common.entity.data.BaseSequenceDO;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.mapper.BaseSequenceMapper;
import com.kuretru.api.common.service.BaseSequenceService;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseSequenceServiceImpl<M extends BaseSequenceMapper<D>, D extends BaseSequenceDO, T extends BaseDTO> extends BaseServiceImpl<M, D, T> implements BaseSequenceService<T> {

    protected BaseSequenceServiceImpl(M mapper, Class<D> doClass, Class<T> dtoClass) {
        super(mapper, doClass, dtoClass);
    }

    @Override
    public int getMaxSequence() {
        Integer result = mapper.getMaxSequence();
        return null == result ? 0 : result;
    }

    @Override
    public List<T> list() {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.orderByAsc("sequence");
        List<D> records = mapper.selectList(queryWrapper);
        return doToDto(records);
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
        return get(data.getId());
    }

}
