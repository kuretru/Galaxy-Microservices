package com.kuretru.microservices.web.v2.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.ColumnCache;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.kuretru.microservices.common.entity.enums.BaseEnum;
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
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.ResolvableType;
import org.springframework.util.StringUtils;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

@Slf4j
public abstract class BaseServiceImpl<M extends BaseMapper<D>, D extends BaseDO, T extends BaseDTO, Q>
        implements BaseService<T, Q> {

    private static final String QUERY_IN_SUFFIX = "IN";
    private static final String QUERY_LIKE_SUFFIX = "LIKE";
    private static final String QUERY_BEGIN_SUFFIX = "BEGIN";
    private static final String QUERY_END_SUFFIX = "END";

    protected final M mapper;
    protected final BaseEntityMapper<D, T> entityMapper;
    protected final Class<D> doClass;
    protected final Class<T> dtoClass;
    protected final Class<Q> queryClass;

    @SuppressWarnings("unchecked")
    public BaseServiceImpl(M mapper, BaseEntityMapper<D, T> entityMapper) {
        this.mapper = mapper;
        this.entityMapper = entityMapper;
        ResolvableType type = ResolvableType.forClass(getClass()).as(BaseServiceImpl.class);
        this.doClass = (Class<D>) type.getGeneric(1).resolve();
        this.dtoClass = (Class<T>) type.getGeneric(2).resolve();
        this.queryClass = (Class<Q>) type.getGeneric(3).resolve();
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
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        try {
            BeanInfo beanInfo = Introspector.getBeanInfo(queryClass);
            List<PropertyDescriptor> descriptors = Arrays.stream(beanInfo.getPropertyDescriptors()).filter(p -> {
                String name = p.getName();
                return !"class".equals(name);
            }).toList();

            // 用MyBatis自带的缓存，将驼峰映射为下划线列名
            Map<String, ColumnCache> columns = LambdaUtils.getColumnMap(doClass);
            for (PropertyDescriptor descriptor : descriptors) {
                Method readMethod = descriptor.getReadMethod();
                if (readMethod == null) {
                    continue;
                }
                Object value = readMethod.invoke(query);
                if (value == null) {
                    continue;
                }

                String filedName = descriptor.getName().toUpperCase();
                if (filedName.endsWith(QUERY_IN_SUFFIX)) {
                    filedName = com.kuretru.microservices.common.utils.StringUtils.trimSuffix(filedName, QUERY_IN_SUFFIX);
                    String columnName = columns.get(filedName).getColumn();
                    if (StringUtils.hasText(columnName)) {
                        if (value instanceof Collection<?> collection) {
                            List<Object> values = collection.stream()
                                    .map(this::toDatabaseValue)
                                    .toList();
                            if (!values.isEmpty()) {
                                queryWrapper.in(columnName, values);
                            }
                        } else {
                            log.warn("BaseServiceImpl.buildQueryWrapper: IN运算符不支持的类型{}", filedName);
                        }
                    }
                } else if (filedName.endsWith(QUERY_LIKE_SUFFIX)) {
                    filedName = com.kuretru.microservices.common.utils.StringUtils.trimSuffix(filedName, QUERY_LIKE_SUFFIX);
                    String columnName = columns.get(filedName).getColumn();
                    if (StringUtils.hasText(columnName)) {
                        queryWrapper.like(columnName, toDatabaseValue(value));
                    }
                } else if (filedName.endsWith(QUERY_BEGIN_SUFFIX)) {
                    filedName = com.kuretru.microservices.common.utils.StringUtils.trimSuffix(filedName, QUERY_BEGIN_SUFFIX);
                    String columnName = columns.get(filedName).getColumn();
                    if (StringUtils.hasText(columnName)) {
                        queryWrapper.ge(columnName, toDatabaseValue(value));
                    }
                } else if (filedName.endsWith(QUERY_END_SUFFIX)) {
                    filedName = com.kuretru.microservices.common.utils.StringUtils.trimSuffix(filedName, QUERY_END_SUFFIX);
                    String columnName = columns.get(filedName).getColumn();
                    if (StringUtils.hasText(columnName)) {
                        queryWrapper.le(columnName, toDatabaseValue(value));
                    }
                } else {
                    String columnName = columns.get(filedName).getColumn();
                    if (StringUtils.hasText(columnName)) {
                        queryWrapper.eq(columnName, toDatabaseValue(value));
                    }
                }
            }
        } catch (IntrospectionException | IllegalAccessException | InvocationTargetException e) {
            log.error("构建QueryWrapper时抛出异常：{}", e.getMessage());
        }
        return queryWrapper;
    }

    private Object toDatabaseValue(Object value) {
        return switch (value) {
            case UUID uuid -> uuid.toString();
            case BaseEnum<?> baseEnum -> baseEnum.getCode();
            default -> value;
        };
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

    protected T afterSave(D record) throws ServiceException {
        return entityMapper.doToDto(record);
    }

    @Override
    public T save(T record) throws ServiceException {
        D data = beforeSave(record);
        mapper.insert(data);
        return afterSave(data);
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

    protected T afterUpdate(D record) throws ServiceException {
        return entityMapper.doToDto(record);
    }

    @Override
    public T update(T record) throws ServiceException {
        D data = beforeUpdate(record);
        int rows = mapper.updateById(data);
        if (rows == 0) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        } else if (rows != 1) {
            throw ServiceException.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "发现多个相同业务主键");
        }
        return afterUpdate(mapper.selectById(data.getId()));
    }
    // endregion

    // region remove
    protected D beforeRemove(Long id) throws ServiceException {
        return mapper.selectById(id);
    }

    protected void afterRemove(D record) throws ServiceException {

    }

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
