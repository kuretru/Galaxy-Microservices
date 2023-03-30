package com.kuretru.microservices.web.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.kuretru.microservices.web.constant.code.ServiceErrorCodes;
import com.kuretru.microservices.web.entity.data.BaseSequenceDO;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.mapper.BaseSequenceMapper;
import com.kuretru.microservices.web.service.BaseSequenceService;
import org.mapstruct.Mapping;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseSequenceServiceImpl<M extends BaseSequenceMapper<D>, D extends BaseSequenceDO, T extends BaseDTO, Q> extends BaseServiceImpl<M, D, T, Q> implements BaseSequenceService<T, Q> {

    public BaseSequenceServiceImpl(M mapper, BaseEntityMapper<D, T> entityMapper) {
        super(mapper, entityMapper);
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
            throw ServiceException.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "更新条数不正确");
        }
    }

    @Override
    public T save(T record) throws ServiceException {
        verifyDTO(record);

        UUID uuid = UUID.randomUUID();
        while (get(uuid) != null) {
            uuid = UUID.randomUUID();
        }

        D data = entityMapper.dtoToDo(record);
        addCreateTime(data, uuid);
        data.setSequence(getMaxSequence() + 1);
        mapper.insert(data);
        return super.get(data.getId());
    }

    @Override
    protected void addDefaultOrderBy(QueryWrapper<D> queryWrapper) {
        queryWrapper.orderByAsc("sequence");
    }

    public interface BaseSequenceEntityMapper<D extends BaseSequenceDO, T extends BaseDTO> extends BaseEntityMapper<D, T> {

        /**
         * 将数据传输实体转换为数据实体
         *
         * @param record 数据传输实体
         * @return 数据实体
         */
        @Mapping(source = "id", target = "uuid")
        @Mapping(target = "id", ignore = true)
        @Mapping(target = "createTime", ignore = true)
        @Mapping(target = "updateTime", ignore = true)
        @Mapping(target = "sequence", ignore = true)
        D dtoToDo(T record);

    }

}
