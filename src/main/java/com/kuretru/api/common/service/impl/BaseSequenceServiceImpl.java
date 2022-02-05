package com.kuretru.api.common.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.kuretru.api.common.constant.code.ServiceErrorCodes;
import com.kuretru.api.common.entity.data.BaseSequenceDO;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.mapper.BaseSequenceMapper;
import com.kuretru.api.common.service.BaseSequenceService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseSequenceServiceImpl<M extends BaseSequenceMapper<D>, D extends BaseSequenceDO, T extends BaseDTO, Q> extends BaseServiceImpl<M, D, T, Q> implements BaseSequenceService<T, Q> {

    public BaseSequenceServiceImpl(M mapper, Class<D> doClass, Class<T> dtoClass) {
        super(mapper, doClass, dtoClass);
    }

    @Override
    public int getMaxSequence() {
        Integer result = mapper.getMaxSequence(null);
        return null == result ? 0 : result;
    }

    @Override
    public void reorder(List<UUID> uuidList) throws ServiceException {
        if (uuidList.isEmpty()) {
            return;
        }

        List<D> records = verifyUuidList(uuidList);
        int length = uuidList.size();

        // 将所有记录的Sequence排序
        List<Integer> sequences = new ArrayList<>(length);
        records.forEach(record -> sequences.add(record.getSequence()));
        Collections.sort(sequences);

        // 按新顺序一一赋值
        List<D> newRecords = new ArrayList<>(length);
        for (int i = 0; i < length; i++) {
            D record = buildDOInstance();
            record.setUuid(uuidList.get(i).toString());
            record.setSequence(sequences.get(i));
            newRecords.add(record);
        }

        // 更新顺序
        Integer result = mapper.updateSequenceByUuids(newRecords);
        if (result == null || result != length) {
            throw new ServiceException.InternalServerError(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "更新条数不正确");
        }
    }

    @Override
    public synchronized T save(T record) throws ServiceException {
        UUID uuid = UUID.randomUUID();
        if (get(uuid) != null) {
            throw new ServiceException.InternalServerError(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "产生了已存在的UUID，请重新提交请求");
        }

        D data = dtoToDo(record);
        addCreateTime(data, uuid);
        data.setSequence(getMaxSequence() + 1);
        mapper.insert(data);
        return super.get(data.getId());
    }

    @Override
    protected void addDefaultOrderBy(QueryWrapper<D> queryWrapper) {
        queryWrapper.orderByAsc("sequence");
    }

}
