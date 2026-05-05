package com.kuretru.microservices.web.v2.service.impl;

import com.kuretru.microservices.web.entity.interfaces.Sequenced;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.v2.entity.data.BaseDO;
import com.kuretru.microservices.web.v2.entity.mapper.BaseEntityMapper;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.v2.mapper.BaseSequencedMapper;
import com.kuretru.microservices.web.v2.service.BaseSequencedService;

import java.util.List;
import java.util.UUID;

public abstract class BaseSequencedServiceImpl<M extends BaseSequencedMapper<D>, D extends BaseDO & Sequenced, T extends BaseDTO, Q> extends BaseServiceImpl<M, D, T, Q> implements BaseSequencedService<T, Q> {

    public BaseSequencedServiceImpl(M mapper, BaseEntityMapper<D, T> entityMapper) {
        super(mapper, entityMapper);
    }

    @Override
    public int getMaxSequence(T record) {
        Integer result = mapper.getMaxSequence(null);
        return null == result ? 0 : result;
    }

    @Override
    public void reorder(List<UUID> uuidList) throws ServiceException {

    }

    @Override
    protected D beforeSave(T record) throws ServiceException {
        D data = super.beforeSave(record);
        data.setSequence(getMaxSequence(record));
        return data;
    }

}
