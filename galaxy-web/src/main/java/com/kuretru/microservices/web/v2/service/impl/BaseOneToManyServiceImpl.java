package com.kuretru.microservices.web.v2.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.microservices.web.entity.interfaces.Sequenced;
import com.kuretru.microservices.web.v2.entity.data.BaseDO;
import com.kuretru.microservices.web.v2.entity.mapper.BaseEntityMapper;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.v2.service.BaseOneToManyService;
import org.springframework.core.ResolvableType;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public abstract class BaseOneToManyServiceImpl<M extends BaseMapper<D>, D extends BaseDO, T extends BaseDTO>
        implements BaseOneToManyService<T> {

    protected final M mapper;
    protected final BaseEntityMapper<D, T> entityMapper;
    protected final Class<D> doClass;
    protected final Class<T> dtoClass;

    @SuppressWarnings("unchecked")
    public BaseOneToManyServiceImpl(M mapper, BaseEntityMapper<D, T> entityMapper) {
        this.mapper = mapper;
        this.entityMapper = entityMapper;
        ResolvableType type = ResolvableType.forClass(getClass()).as(BaseOneToManyServiceImpl.class);
        this.doClass = (Class<D>) type.getGeneric(1).resolve();
        this.dtoClass = (Class<T>) type.getGeneric(2).resolve();
    }

    protected abstract String getParentIdColumn();

    protected abstract Long getParentId(T record);

    protected abstract void setParentId(Long parentId, T record);

    protected abstract boolean bizEqual(D oldRecord, T newRecord);

    // region 通用方法

    /**
     * 为QueryWrapper设置排序依据
     *
     * @param queryWrapper QueryWrapper
     */
    protected void applyDefaultOrderBy(QueryWrapper<D> queryWrapper) {
        if (Sequenced.class.isAssignableFrom(doClass)) {
            queryWrapper.orderByAsc("sequence");
        }
        queryWrapper.orderByAsc("id");
    }

    protected QueryWrapper<D> buildQueryWrapper(Long parentId) {
        var queryWrapper = new QueryWrapper<D>();
        queryWrapper.eq(getParentIdColumn(), parentId);
        applyDefaultOrderBy(queryWrapper);
        return queryWrapper;
    }

    protected QueryWrapper<D> buildQueryWrapper(List<Long> parentIdList) {
        var queryWrapper = new QueryWrapper<D>();
        queryWrapper.in(getParentIdColumn(), parentIdList);
        applyDefaultOrderBy(queryWrapper);
        return queryWrapper;
    }
    // endregion


    @Override
    public List<T> listByParentId(Long parentId) {
        var queryWrapper = buildQueryWrapper(parentId);
        var records = mapper.selectList(queryWrapper);
        return entityMapper.doToDto(records);
    }

    @Override
    public Map<Long, List<T>> listByParentId(List<Long> parentIdList) {
        var queryWrapper = buildQueryWrapper(parentIdList);
        var records = mapper.selectList(queryWrapper);
        var dtoRecords = entityMapper.doToDto(records);
        return dtoRecords.stream().collect(Collectors.groupingBy(this::getParentId));
    }

    @Override
    public List<T> syncByParentId(Long parentId, List<T> newRecords) {
        if (newRecords == null || newRecords.isEmpty()) {
            return newRecords;
        }

        var queryWrapper = buildQueryWrapper(parentId);
        var oldRecords = mapper.selectList(queryWrapper);
        Map<Long, D> oldRecordsMap = oldRecords.stream().collect(Collectors.toMap(BaseDO::getId, Function.identity()));

        List<D> insertList = new ArrayList<>();
        List<D> updateList = new ArrayList<>();

        for (var record : newRecords) {
            setParentId(parentId, record);
            if (oldRecordsMap.containsKey(record.getId())) {
                if (!bizEqual(oldRecordsMap.get(record.getId()), record)) {
                    updateList.add(entityMapper.dtoToDo(record));
                }
                oldRecordsMap.remove(record.getId());
            } else {
                insertList.add(entityMapper.dtoToDo(record));
            }
        }
        List<D> deleteList = new ArrayList<>(oldRecordsMap.values());

        if (!insertList.isEmpty()) {
            mapper.insert(insertList);
        }
        if (!updateList.isEmpty()) {
            mapper.updateById(updateList);
        }
        if (!deleteList.isEmpty()) {
            mapper.deleteByIds(deleteList);
        }

        var result = mapper.selectList(queryWrapper);
        return entityMapper.doToDto(result);
    }

    @Override
    public void removeByParentId(Long parentId) {
        var queryWrapper = buildQueryWrapper(parentId);
        mapper.delete(queryWrapper);
    }

}
