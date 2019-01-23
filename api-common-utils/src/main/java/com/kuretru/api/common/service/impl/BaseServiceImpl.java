package com.kuretru.api.common.service.impl;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.toolkit.SqlHelper;
import com.github.dozermapper.core.Mapper;
import com.kuretru.api.common.service.BaseService;
import com.kuretru.api.common.util.PojoUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.lang.reflect.ParameterizedType;
import java.util.List;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public abstract class BaseServiceImpl<M extends BaseMapper<D>, D, V, T> implements BaseService<V, T> {
    @Autowired
    protected M mapper;

    @Autowired
    protected Mapper beanConverter;

    @Override
    public V get(Integer id) {
        D record = mapper.selectById(id);
        return beanConverter.map(record, getVOClass());
    }

    @Override
    public List<V> list() {
        List<D> records = mapper.selectList(Wrappers.emptyWrapper());
        return PojoUtils.map(records, getVOClass());
    }

    @Override
    public int count() {
        return SqlHelper.retCount(mapper.selectCount(Wrappers.emptyWrapper()));
    }

    @Override
    public V save(T record) {
        return null;
    }

    @Override
    public int remove(Integer id) {
        return 0;
    }

    @Override
    public V update(T record) {
        return null;
    }

    protected Class<V> getVOClass() {
        ParameterizedType type = (ParameterizedType) this.getClass().getGenericSuperclass();
        return PojoUtils.cast(type.getActualTypeArguments()[2]);
    }

}
