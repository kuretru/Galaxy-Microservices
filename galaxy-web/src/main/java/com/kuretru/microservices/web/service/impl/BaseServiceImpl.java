package com.kuretru.microservices.web.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.ColumnCache;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.kuretru.microservices.common.entity.enums.BaseEnum;
import com.kuretru.microservices.web.constant.code.ServiceErrorCodes;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.entity.PaginationQuery;
import com.kuretru.microservices.web.entity.PaginationResponse;
import com.kuretru.microservices.web.entity.data.BaseDO;
import com.kuretru.microservices.web.entity.mapper.BaseEntityMapper;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.service.BaseService;
import lombok.SneakyThrows;
import org.springframework.util.StringUtils;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.time.Instant;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

/**
 * 业务逻辑层基类，继承此类即可获得一些基础方法
 * 泛型：M->实体对应的数据访问层，D->实体对应的数据对象，T->实体对应的数据传输对象，Q->实体对应的查询对象
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseServiceImpl<M extends BaseMapper<D>, D extends BaseDO, T extends BaseDTO, Q>
        implements BaseService<T, Q> {

    protected final M mapper;
    protected final BaseEntityMapper<D, T> entityMapper;
    protected final Class<D> doClass;
    protected final Class<T> dtoClass;
    protected final Class<Q> queryClass;

    @SuppressWarnings("unchecked")
    public BaseServiceImpl(M mapper, BaseEntityMapper<D, T> entityMapper) {
        this.mapper = mapper;
        this.entityMapper = entityMapper;
        ParameterizedType baseType = (ParameterizedType)this.getClass().getGenericSuperclass();
        doClass = (Class<D>)baseType.getActualTypeArguments()[1];
        dtoClass = (Class<T>)baseType.getActualTypeArguments()[2];
        queryClass = (Class<Q>)baseType.getActualTypeArguments()[3];
    }

    @Override
    public T get(Long id) {
        D record = mapper.selectById(id);
        if (record != null) {
            verifyDO(record);
        }
        return entityMapper.doToDto(record);
    }

    @Override
    public T get(UUID uuid) {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("uuid", uuid.toString());
        D record = mapper.selectOne(queryWrapper);
        if (record != null) {
            verifyDO(record);
        }
        return entityMapper.doToDto(record);
    }

    protected List<D> list(List<UUID> uuidList) {
        List<String> ids = uuidList.stream().map(UUID::toString).collect(Collectors.toList());
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.in("uuid", ids);
        addDefaultOrderBy(queryWrapper);
        return mapper.selectList(queryWrapper);
    }

    protected List<T> list(QueryWrapper<D> queryWrapper) {
        return entityMapper.doToDto(mapper.selectList(queryWrapper));
    }

    @Override
    public List<T> list() {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        addDefaultOrderBy(queryWrapper);
        return list(queryWrapper);
    }

    @Override
    public List<T> list(Q query) {
        verifyQuery(query);
        QueryWrapper<D> queryWrapper = buildQueryWrapper(query);
        addDefaultOrderBy(queryWrapper);
        return list(queryWrapper);
    }

    protected PaginationResponse<T> list(PaginationQuery pagination, QueryWrapper<D> queryWrapper) {
        Page<D> page = new Page<>(pagination.getCurrent(), pagination.getPageSize());
        page = mapper.selectPage(page, queryWrapper);
        List<T> records = entityMapper.doToDto(page.getRecords());
        return new PaginationResponse<>(records, page.getCurrent(), page.getSize(), page.getTotal());
    }

    @Override
    public PaginationResponse<T> list(PaginationQuery paginationQuery) {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        addDefaultOrderBy(queryWrapper);
        return list(paginationQuery, queryWrapper);
    }

    @Override
    public PaginationResponse<T> list(PaginationQuery pagination, Q query) {
        verifyQuery(query);
        QueryWrapper<D> queryWrapper = buildQueryWrapper(query);
        addDefaultOrderBy(queryWrapper);
        return list(pagination, queryWrapper);
    }

    @Override
    public int count() {
        Long result = mapper.selectCount(null);
        return null == result ? 0 : Math.toIntExact(result);
    }

    @Override
    public T save(T record) throws ServiceException {
        verifyDTO(record);
        if (findUniqueRecord(record) != null) {
            throw new ServiceException(UserErrorCodes.UNIQUENESS_CHECK_FAILED, "已存在相同的记录");
        }

        UUID uuid = UUID.randomUUID();
        while (get(uuid) != null) {
            uuid = UUID.randomUUID();
        }

        D data = entityMapper.dtoToDo(record);
        addCreateTime(data, uuid);
        mapper.insert(data);
        return get(data.getId());
    }

    @Override
    public T update(T record) throws ServiceException {
        verifyDTO(record);
        T uniqueRecord = findUniqueRecord(record);
        if (uniqueRecord != null && !uniqueRecord.getId().equals(record.getId())) {
            throw new ServiceException(UserErrorCodes.UNIQUENESS_CHECK_FAILED, "已存在相同的记录");
        }

        D data = entityMapper.dtoToDo(record);
        data.setUpdateTime(Instant.now());
        UpdateWrapper<D> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("uuid", data.getUuid());
        int rows = mapper.update(data, updateWrapper);
        if (0 == rows) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        } else if (1 != rows) {
            throw ServiceException.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "发现多个相同业务主键");
        }
        return get(record.getId());
    }

    @Override
    public void remove(UUID uuid) throws ServiceException {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("uuid", uuid.toString());
        int rows = mapper.delete(queryWrapper);
        if (0 == rows) {
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        } else if (1 != rows) {
            throw ServiceException.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "发现多个相同业务主键");
        }
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

            for (PropertyDescriptor descriptor : descriptors) {
                Method readMethod = descriptor.getReadMethod();
                if (readMethod == null) {
                    continue;
                }
                Object value = readMethod.invoke(query);
                if (value == null) {
                    continue;
                }

                // 用MyBatis自带的缓存，将驼峰映射为下划线列名
                Map<String, ColumnCache> columns = LambdaUtils.getColumnMap(doClass);
                String columnName = columns.get(descriptor.getName().toUpperCase()).getColumn();

                if (value instanceof String && StringUtils.hasText((String)value)) {
                    // String类型：like '%xxx%'
                    queryWrapper.like(columnName, value);
                } else if (value instanceof UUID) {
                    // UUID类型：= UUID
                    queryWrapper.eq(columnName, value.toString());
                } else if (value instanceof Boolean) {
                    queryWrapper.eq(columnName, value);
                } else if (value instanceof BaseEnum) {
                    // 枚举类型：= 枚举编号
                    queryWrapper.eq(columnName, ((BaseEnum<?>)value).getCode());
                } else if (value instanceof LocalDate) {
                    // 日期类型：= 日期
                    queryWrapper.eq(columnName, value);
                }
            }
        } catch (IntrospectionException | IllegalAccessException | InvocationTargetException e) {
            e.printStackTrace();
        }
        return queryWrapper;
    }

    /**
     * 为QueryWrapper设置默认排序依据
     *
     * @param queryWrapper QueryWrapper
     */
    protected void addDefaultOrderBy(QueryWrapper<D> queryWrapper) {
        queryWrapper.orderByAsc("id");
    }

    /**
     * 增加记录创建时间
     *
     * @param record 记录
     * @param uuid   UUID
     */
    protected void addCreateTime(D record, UUID uuid) {
        record.setUuid(uuid.toString());
        Instant now = Instant.now();
        record.setCreateTime(now);
        record.setUpdateTime(now);
    }

    /**
     * 查询记录时，在业务层面验证传入的Query是否合法
     *
     * @param query Query
     * @throws ServiceException 不合法时抛出业务异常
     */
    protected void verifyQuery(Q query) throws ServiceException {

    }

    /**
     * 查询单条记录时，在业务层面进行横向鉴权
     *
     * @param record 查询出的单条记录
     * @throws ServiceException 不合法时抛出业务异常
     */
    protected void verifyDO(D record) throws ServiceException {

    }

    /**
     * 增加或修改记录时，在业务层面验证传入的DTO内容是否合法
     *
     * @param record DTO
     * @throws ServiceException 不合法时抛出业务异常
     */
    protected void verifyDTO(T record) throws ServiceException {

    }

    /**
     * 增加或修改记录时，查找是否已存在相同的记录，应尽量保证查询的字段唯一索引存在
     * 默认不检查唯一性
     *
     * @param record DTO
     * @return 记录不存在时返回null，否则返回对应记录DTO，更新时用于判断是否为记录本身
     */
    protected T findUniqueRecord(T record) {
        return null;
    }

    /**
     * 验证给定UUID列表是否存在
     *
     * @param uuidList UUID列表
     * @return 给定UUID列表对应的实体
     * @throws ServiceException 部分UUID不存在时抛出异常
     */
    protected List<D> verifyUuidList(List<UUID> uuidList) throws ServiceException {
        List<D> records = list(uuidList);
        if (records.size() > uuidList.size()) {
            throw ServiceException.build(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "发现多个相同业务主键");
        } else if (records.size() < uuidList.size()) {
            //TODO 返回所有不存在的列表
            throw ServiceException.build(UserErrorCodes.REQUEST_PARAMETER_ERROR, "部分不存在");
        }
        return records;
    }

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
