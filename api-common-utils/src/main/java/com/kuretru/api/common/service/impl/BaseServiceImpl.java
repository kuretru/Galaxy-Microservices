package com.kuretru.api.common.service.impl;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.api.common.entity.data.BaseDO;
import com.kuretru.api.common.service.BaseService;

import java.util.ArrayList;
import java.util.List;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public abstract class BaseServiceImpl<M extends BaseMapper<D>, D extends BaseDO, T> implements BaseService<M, D, T> {

    protected M mapper;

    @Override
    public T get(Long id) {
        D record = mapper.selectById(id);
        return doToDTO(record);
    }

    @Override
    public List<T> list() {
        List<D> records = mapper.selectList(null);
        return doToDTO(records);
    }

    @Override
    public int count() {
        Integer result = mapper.selectCount(null);
        return null == result ? 0 : result;
    }

    @Override
    public T save(T record) {
        D data = dtoToDO(record);
        data.addCrateTime();
        mapper.insert(data);
        return get(data.getId());
    }

    @Override
    public int remove(Long id) {
        return mapper.deleteById(id);
    }

    @Override
    public T update(T record) {
        D data = dtoToDO(record);
        mapper.updateById(data);
        return get(data.getId());
    }

    @Override
    public List<T> doToDTO(List<D> records) {
        List<T> result = new ArrayList<>(records.size());
        for (D record : records) {
            result.add(doToDTO(record));
        }
        return result;
    }

    @Override
    public List<D> dtoToDO(List<T> records) {
        List<D> result = new ArrayList<>(records.size());
        for (T record : records) {
            result.add(dtoToDO(record));
        }
        return result;
    }

}
