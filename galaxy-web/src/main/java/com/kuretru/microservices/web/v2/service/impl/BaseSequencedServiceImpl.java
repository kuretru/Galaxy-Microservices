package com.kuretru.microservices.web.v2.service.impl;

import com.kuretru.microservices.web.entity.interfaces.Sequenced;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.v2.entity.data.BaseDO;
import com.kuretru.microservices.web.v2.entity.mapper.BaseEntityMapper;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.v2.mapper.BaseSequencedMapper;
import com.kuretru.microservices.web.v2.service.BaseService;
import com.kuretru.microservices.web.v2.service.SequencedService;

import java.util.List;

public abstract class BaseSequencedServiceImpl<M extends BaseSequencedMapper<D>, D extends BaseDO & Sequenced, T extends BaseDTO, Q>
        extends BaseServiceImpl<M, D, T, Q> implements BaseService<T, Q>, SequencedService<T> {

    public BaseSequencedServiceImpl(M mapper, BaseEntityMapper<D, T> entityMapper) {
        super(mapper, entityMapper);
    }

    @Override
    public int getMaxSequence(T record) {
        Integer result = mapper.getMaxSequence(null);
        return null == result ? 0 : result;
    }

    @Override
    public void reorder(List<Integer> idList) throws ServiceException {

    }

    @Override
    protected D beforeSave(T record) throws ServiceException {
        D data = super.beforeSave(record);
        data.setSequence(getMaxSequence(record) + 1);
        return data;
    }

}
