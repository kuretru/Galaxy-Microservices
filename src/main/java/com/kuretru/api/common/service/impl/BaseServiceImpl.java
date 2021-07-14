package com.kuretru.api.common.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.ColumnCache;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.kuretru.api.common.constant.code.ServiceErrorCodes;
import com.kuretru.api.common.constant.code.UserErrorCodes;
import com.kuretru.api.common.entity.PaginationQuery;
import com.kuretru.api.common.entity.PaginationResponse;
import com.kuretru.api.common.entity.data.BaseDO;
import com.kuretru.api.common.entity.enums.BaseEnum;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.service.BaseService;
import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;
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
import java.util.*;
import java.util.stream.Collectors;

/**
 * 业务逻辑层基类，继承此类即可获得一些基础方法
 * 泛型：M->实体对应的数据访问层，D->实体对应的数据对象，T->实体对应的数据传输对象，Q->实体对应的查询对象
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseServiceImpl<M extends BaseMapper<D>, D extends BaseDO, T extends BaseDTO, Q> implements BaseService<T, Q> {

    protected final M mapper;
    protected final Class<D> doClass;
    protected final Class<T> dtoClass;

    public BaseServiceImpl(M mapper, Class<D> doClass, Class<T> dtoClass) {
        this.mapper = mapper;
        this.doClass = doClass;
        this.dtoClass = dtoClass;
    }

    @Override
    public T get(Long id) {
        D record = mapper.selectById(id);
        return doToDto(record);
    }

    @Override
    public T get(UUID uuid) {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("uuid", uuid.toString());
        D record = mapper.selectOne(queryWrapper);
        return doToDto(record);
    }

    protected List<D> list(QueryWrapper<D> queryWrapper) {
        return mapper.selectList(queryWrapper);
    }

    protected List<D> list(List<UUID> uuidList) {
        List<String> ids = uuidList.stream().map(UUID::toString).collect(Collectors.toList());
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.in("uuid", ids);
        addDefaultOrderBy(queryWrapper);
        return mapper.selectList(queryWrapper);
    }

    @Override
    public List<T> list() {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        addDefaultOrderBy(queryWrapper);
        return doToDto(list(queryWrapper));
    }

    @Override
    public List<T> list(Q query) {
        QueryWrapper<D> queryWrapper = buildQueryWrapper(query);
        addDefaultOrderBy(queryWrapper);
        return doToDto(list(queryWrapper));
    }

    protected PaginationResponse<T> list(PaginationQuery pagination, QueryWrapper<D> queryWrapper) {
        Page<D> page = new Page<>(pagination.getCurrent(), pagination.getPageSize());
        page = mapper.selectPage(page, queryWrapper);
        List<T> records = doToDto(page.getRecords());
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
        QueryWrapper<D> queryWrapper = buildQueryWrapper(query);
        addDefaultOrderBy(queryWrapper);
        return list(pagination, queryWrapper);
    }

    @Override
    public int count() {
        Integer result = mapper.selectCount(null);
        return null == result ? 0 : result;
    }

    protected void addCreateTime(D record, UUID uuid) {
        record.setUuid(uuid.toString());
        Instant now = Instant.now();
        record.setCreateTime(now);
        record.setUpdateTime(now);
    }

    @Override
    public synchronized T save(T record) throws ServiceException {
        UUID uuid = UUID.randomUUID();
        if (get(uuid) != null) {
            throw new ServiceException.InternalServerError(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "产生了已存在的UUID，请重新提交请求");
        }

        D data = dtoToDo(record);
        addCreateTime(data, uuid);
        mapper.insert(data);
        return get(data.getId());
    }

    @Override
    public T update(T record) throws ServiceException {
        D data = dtoToDo(record);
        data.setUpdateTime(Instant.now());
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("uuid", data.getUuid());
        int rows = mapper.update(data, queryWrapper);
        if (0 == rows) {
            throw new ServiceException.NotFound(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        } else if (1 != rows) {
            throw new ServiceException.InternalServerError(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "发现多个相同业务主键");
        }
        return get(record.getId());
    }

    @Override
    public void remove(UUID uuid) throws ServiceException {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("uuid", uuid.toString());
        int rows = mapper.delete(queryWrapper);
        if (0 == rows) {
            throw new ServiceException.NotFound(UserErrorCodes.REQUEST_PARAMETER_ERROR, "指定资源不存在");
        } else if (1 != rows) {
            throw new ServiceException.InternalServerError(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "发现多个相同业务主键");
        }
    }

    /**
     * 将数据实体转换为数据传输实体
     *
     * @param record 数据实体
     * @return 数据传输实体
     */
    protected T doToDto(D record) {
        if (record == null) {
            return null;
        }
        T result = buildDTOInstance();
        BeanUtils.copyProperties(record, result);
        result.setId(UUID.fromString(record.getUuid()));
        return result;
    }

    /**
     * 将数据实体批量转换为数据传输实体
     *
     * @param records 数据实体列表
     * @return 数据传输实体列表
     */
    protected List<T> doToDto(List<D> records) {
        if (records == null) {
            return null;
        }
        List<T> result = new ArrayList<>(records.size());
        for (D record : records) {
            result.add(doToDto(record));
        }
        return result;
    }

    /**
     * 将数据传输实体转换为数据实体
     *
     * @param record 数据传输实体
     * @return 数据实体
     */
    protected D dtoToDo(T record) {
        if (record == null) {
            return null;
        }
        D result = buildDOInstance();
        BeanUtils.copyProperties(record, result);
        if (record.getId() != null) {
            result.setUuid(record.getId().toString());
        }
        return result;
    }

    /**
     * 将数据传输实体批量转换为数据实体
     *
     * @param records 数据传输实体列表
     * @return 数据实体列表
     */
    protected List<D> dtoToDo(List<T> records) {
        if (records == null) {
            return null;
        }
        List<D> result = new ArrayList<>(records.size());
        for (T record : records) {
            result.add(dtoToDo(record));
        }
        return result;
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

    /** 根据查询实体构建QueryWrapper */
    protected QueryWrapper<D> buildQueryWrapper(Q query) {
        ParameterizedType baseType = (ParameterizedType)this.getClass().getGenericSuperclass();
        Class<?> queryClass = (Class<?>)baseType.getActualTypeArguments()[3];

        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        try {
            BeanInfo beanInfo = Introspector.getBeanInfo(queryClass);
            List<PropertyDescriptor> descriptors = Arrays.stream(beanInfo.getPropertyDescriptors()).filter(p -> {
                String name = p.getName();
                return !"class".equals(name);
            }).collect(Collectors.toList());

            for (PropertyDescriptor descriptor : descriptors) {
                Method readMethod = descriptor.getReadMethod();
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
                } else if (value instanceof BaseEnum) {
                    // 枚举类型：= 枚举编号
                    queryWrapper.eq(columnName, ((BaseEnum)value).getCode());
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

    protected void addDefaultOrderBy(QueryWrapper<D> queryWrapper) {
        queryWrapper.orderByAsc("id");
    }

}
