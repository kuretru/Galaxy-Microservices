package com.kuretru.microservices.web.v2.service.support;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.ColumnCache;
import com.kuretru.microservices.common.entity.enums.BaseEnum;
import com.kuretru.microservices.web.v2.entity.data.BaseDO;
import lombok.extern.slf4j.Slf4j;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

@Slf4j
public class QueryWrapperBuilder<D extends BaseDO, Q> {

    private static final String QUERY_IN_SUFFIX = "IN";
    private static final String QUERY_LIKE_SUFFIX = "LIKE";
    private static final String QUERY_BEGIN_SUFFIX = "BEGIN";
    private static final String QUERY_END_SUFFIX = "END";

    private final Class<D> doClass;
    private final Class<Q> queryClass;

    public QueryWrapperBuilder(Class<D> doClass, Class<Q> queryClass) {
        this.doClass = doClass;
        this.queryClass = queryClass;
    }

    /**
     * 根据查询实体构建QueryWrapper
     *
     * @param query 查询实体
     * @return QueryWrapper
     */
    public QueryWrapper<D> build(Q query) {
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
                if (filedName.endsWith(QUERY_BEGIN_SUFFIX)) {
                    // begin一定要在in前面
                    filedName = com.kuretru.microservices.common.utils.StringUtils.trimSuffix(filedName, QUERY_BEGIN_SUFFIX);
                    if (columns.containsKey(filedName)) {
                        String columnName = columns.get(filedName).getColumn();
                        queryWrapper.ge(columnName, toDatabaseValue(value));
                    }
                } else if (filedName.endsWith(QUERY_END_SUFFIX)) {
                    filedName = com.kuretru.microservices.common.utils.StringUtils.trimSuffix(filedName, QUERY_END_SUFFIX);
                    if (columns.containsKey(filedName)) {
                        String columnName = columns.get(filedName).getColumn();
                        queryWrapper.le(columnName, toDatabaseValue(value));
                    }
                } else if (filedName.endsWith(QUERY_IN_SUFFIX)) {
                    filedName = com.kuretru.microservices.common.utils.StringUtils.trimSuffix(filedName, QUERY_IN_SUFFIX);
                    if (columns.containsKey(filedName)) {
                        String columnName = columns.get(filedName).getColumn();
                        if (value instanceof Collection<?> collection) {
                            List<Object> values = collection.stream().map(this::toDatabaseValue).toList();
                            if (!values.isEmpty()) {
                                queryWrapper.in(columnName, values);
                            }
                        } else {
                            log.warn("QueryWrapperBuilder.build: IN运算符不支持的类型{}", filedName);
                        }
                    }
                } else if (filedName.endsWith(QUERY_LIKE_SUFFIX)) {
                    filedName = com.kuretru.microservices.common.utils.StringUtils.trimSuffix(filedName, QUERY_LIKE_SUFFIX);
                    if (columns.containsKey(filedName)) {
                        String columnName = columns.get(filedName).getColumn();
                        queryWrapper.like(columnName, toDatabaseValue(value));
                    }
                } else {
                    if (columns.containsKey(filedName)) {
                        String columnName = columns.get(filedName).getColumn();
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
            case BaseEnum<?> baseEnum -> baseEnum.getValue();
            default -> value;
        };
    }

}
