package com.kuretru.microservices.web.v2.service.impl;

import com.kuretru.microservices.web.constant.code.ServiceErrorCodes;
import com.kuretru.microservices.web.entity.interfaces.Sequenced;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.v2.entity.data.BaseDO;
import com.kuretru.microservices.web.v2.entity.mapper.BaseEntityMapper;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.v2.mapper.BaseSequencedMapper;
import com.kuretru.microservices.web.v2.service.BaseService;
import com.kuretru.microservices.web.v2.service.SequencedService;

import java.util.ArrayList;
import java.util.Collections;
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
    public void reorder(List<Long> idList) throws ServiceException {
        if (idList.isEmpty()) {
            return;
        }

        List<D> records = mapper.selectByIds(idList);
        int length = idList.size();

        // 将所有记录的Sequence排序
        List<Integer> sequences = new ArrayList<>(length);
        records.forEach(record -> sequences.add(record.getSequence()));
        Collections.sort(sequences);

        // 按新顺序一一赋值
        List<D> newRecords = new ArrayList<>(length);
        for (int i = 0; i < length; i++) {
            D record = buildDOInstance();
            record.setId(idList.get(i));
            record.setSequence(sequences.get(i));
            newRecords.add(record);
        }

        // 更新顺序
        Integer result = mapper.updateSequenceById(newRecords);
        if (result == null || result != length) {
            throw ServiceException.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "更新条数不正确");
        }
    }

    @Override
    protected D beforeSave(T record) throws ServiceException {
        D data = super.beforeSave(record);
        data.setSequence(getMaxSequence(record) + 1);
        return data;
    }

}
