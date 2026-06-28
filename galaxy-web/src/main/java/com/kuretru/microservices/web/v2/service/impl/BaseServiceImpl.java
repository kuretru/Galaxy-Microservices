package com.kuretru.microservices.web.v2.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.kuretru.microservices.web.constant.code.ServiceErrorCodes;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.entity.PaginationQuery;
import com.kuretru.microservices.web.entity.PaginationResponse;
import com.kuretru.microservices.web.entity.interfaces.Sequenced;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.v2.entity.data.BaseDO;
import com.kuretru.microservices.web.v2.entity.mapper.BaseEntityMapper;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.v2.service.BaseService;
import com.kuretru.microservices.web.v2.service.support.QueryWrapperBuilder;
import lombok.SneakyThrows;
import org.springframework.core.ResolvableType;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public abstract class BaseServiceImpl<M extends BaseMapper<D>, D extends BaseDO, T extends BaseDTO, Q>
        implements BaseService<T, Q> {

    protected final M mapper;
    protected final BaseEntityMapper<D, T> entityMapper;
    protected final Class<D> doClass;
    protected final Class<T> dtoClass;
    protected final Class<Q> queryClass;
    protected final QueryWrapperBuilder<D, Q> queryWrapperBuilder;

    @SuppressWarnings("unchecked")
    public BaseServiceImpl(M mapper, BaseEntityMapper<D, T> entityMapper) {
        this.mapper = mapper;
        this.entityMapper = entityMapper;
        ResolvableType type = ResolvableType.forClass(getClass()).as(BaseServiceImpl.class);
        this.doClass = (Class<D>) type.getGeneric(1).resolve();
        this.dtoClass = (Class<T>) type.getGeneric(2).resolve();
        this.queryClass = (Class<Q>) type.getGeneric(3).resolve();
        this.queryWrapperBuilder = new QueryWrapperBuilder<>(doClass, queryClass);
    }

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

    /**
     * 增加或修改记录时，查找是否已存在相同的记录，应尽量保证查询的字段唯一索引存在
     * 默认不检查唯一性
     *
     * @param record DO
     * @return 记录不存在时返回null，否则返回对应记录DO，更新时用于判断是否为记录本身
     */
    protected D findDuplicateRecord(T record) {
        return null;
    }

    /**
     * 根据查询实体构建QueryWrapper
     *
     * @param query 查询实体
     * @return QueryWrapper
     */
    protected QueryWrapper<D> buildQueryWrapper(Q query) {
        return queryWrapperBuilder.build(query);
    }
    // endregion

    // region get

    protected T afterGet(D record) throws ServiceException {
        return entityMapper.doToDto(record);
    }

    @Override
    public T get(Long id) throws ServiceException {
        D record = mapper.selectById(id);
        return afterGet(record);
    }
    // endregion

    // region list
    protected QueryWrapper<D> beforeList(Q query) throws ServiceException {
        QueryWrapper<D> queryWrapper = buildQueryWrapper(query);
        applyDefaultOrderBy(queryWrapper);
        return queryWrapper;
    }

    protected List<T> afterList(Q query, List<D> records) throws ServiceException {
        return entityMapper.doToDto(records);
    }

    @Override
    public List<Long> listId(Q query) throws ServiceException {
        QueryWrapper<D> queryWrapper = beforeList(query);
        queryWrapper.select("id");
        return mapper.selectObjs(queryWrapper).stream()
                .map(id -> ((Number) id).longValue())
                .toList();
    }

    @Override
    public List<T> list(Q query) throws ServiceException {
        QueryWrapper<D> queryWrapper = beforeList(query);
        List<D> records = mapper.selectList(queryWrapper);
        return afterList(query, records);
    }

    @Override
    public PaginationResponse<T> list(PaginationQuery pagination, Q query) throws ServiceException {
        QueryWrapper<D> queryWrapper = beforeList(query);
        Page<D> page = new Page<>(pagination.getCurrent(), pagination.getPageSize());
        page = mapper.selectPage(page, queryWrapper);
        List<T> records = afterList(query, page.getRecords());
        return new PaginationResponse<>(records, page.getCurrent(), page.getSize(), page.getTotal());
    }
    // endregion

    // region save

    protected D beforeSave(T record) throws ServiceException {
        if (findDuplicateRecord(record) != null) {
            throw new ServiceException(UserErrorCodes.UNIQUENESS_CHECK_FAILED, "已存在相同的记录");
        }
        return entityMapper.dtoToDo(record);
    }

    protected T afterSave(D record, T raw) throws ServiceException {
        return entityMapper.doToDto(record);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public T save(T record) throws ServiceException {
        D data = beforeSave(record);
        mapper.insert(data);
        return afterSave(data, record);
    }
    // endregion

    // region update
    protected D beforeUpdate(T record) throws ServiceException {
        D duplicatedRecord = findDuplicateRecord(record);
        if (findDuplicateRecord(record) != null && !duplicatedRecord.getId().equals(record.getId())) {
            throw new ServiceException(UserErrorCodes.UNIQUENESS_CHECK_FAILED, "已存在相同的记录");
        }
        return entityMapper.dtoToDo(record);
    }

    protected T afterUpdate(D record, T raw) throws ServiceException {
        return entityMapper.doToDto(record);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public T update(T record) throws ServiceException {
        D data = beforeUpdate(record);
        int rows = mapper.updateById(data);
        if (rows == 0) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        } else if (rows != 1) {
            throw ServiceException.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "发现多个相同业务主键");
        }
        return afterUpdate(mapper.selectById(data.getId()), record);
    }
    // endregion

    // region remove
    protected D beforeRemove(Long id) throws ServiceException {
        return mapper.selectById(id);
    }

    protected void afterRemove(D record) throws ServiceException {

    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void remove(Long id) throws ServiceException {
        D record = beforeRemove(id);
        if (record == null) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        }
        int rows = mapper.deleteById(record);
        if (rows != 1) {
            throw ServiceException.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "发现多个相同业务主键");
        }
        afterRemove(record);
    }
    // endregion


    /**
     * 构建DO的新实例
     *
     * @return DO的新实例
     */
    @SneakyThrows
    protected D buildDOInstance() {
        return doClass.getConstructor().newInstance();
    }

    /**
     * 构建DTO的新实例
     *
     * @return DTO的新实例
     */
    @SneakyThrows
    protected T buildDTOInstance() {
        return dtoClass.getConstructor().newInstance();
    }

}
