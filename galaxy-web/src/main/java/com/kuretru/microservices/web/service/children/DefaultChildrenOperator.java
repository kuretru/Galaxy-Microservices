package com.kuretru.microservices.web.service.children;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.LambdaUtils;
import com.baomidou.mybatisplus.core.toolkit.support.ColumnCache;
import com.kuretru.microservices.web.entity.annotations.ChildrenParentId;
import com.kuretru.microservices.web.entity.interfaces.Children;
import com.kuretru.microservices.web.entity.interfaces.Sequenced;
import com.kuretru.microservices.web.entity.data.BaseDO;
import com.kuretru.microservices.web.entity.mapper.BaseEntityMapper;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.service.support.QueryWrapperBuilder;

import java.lang.reflect.Field;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public class DefaultChildrenOperator<M extends BaseMapper<D>, D extends BaseDO & Children<D>, T extends BaseDTO, Q>
        implements ChildrenOperator<T, Q> {

    private final M mapper;
    private final BaseEntityMapper<D, T> entityMapper;
    private final Class<D> doClass;
    private final Class<T> dtoClass;
    private final Class<Q> queryClass;
    private final QueryWrapperBuilder<D, Q> queryWrapperBuilder;
    private final Field parentIdField;
    private final String parentIdColumn;

    public DefaultChildrenOperator(M mapper, BaseEntityMapper<D, T> entityMapper,
                                   Class<D> doClass, Class<T> dtoClass, Class<Q> queryClass) {
        this.mapper = mapper;
        this.entityMapper = entityMapper;
        this.doClass = doClass;
        this.dtoClass = dtoClass;
        this.queryClass = queryClass;
        this.queryWrapperBuilder = new QueryWrapperBuilder<>(doClass, queryClass);
        this.parentIdField = resolveParentIdField();
        this.parentIdColumn = resolveParentIdColumn(parentIdField);
    }

    // region 通用方法

    private Field resolveParentIdField() {
        List<Field> fields = Arrays.stream(doClass.getDeclaredFields())
                .filter(field -> field.isAnnotationPresent(ChildrenParentId.class))
                .toList();
        if (fields.size() != 1) {
            throw new IllegalStateException("%s必须有且仅有一个字段标记@ChildrenParentId".formatted(doClass.getName()));
        }
        Field field = fields.getFirst();
        if (!Long.class.isAssignableFrom(field.getType())) {
            throw new IllegalStateException("%s的@ChildrenParentId字段类型必须是Long".formatted(doClass.getName()));
        }
        field.setAccessible(true);
        return field;
    }

    private String resolveParentIdColumn(Field field) {
        Map<String, ColumnCache> columns = LambdaUtils.getColumnMap(doClass);
        ColumnCache column = columns.get(field.getName().toUpperCase(Locale.ROOT));
        if (column == null) {
            throw new IllegalStateException("%s的@ChildrenParentId字段未找到对应数据库列".formatted(doClass.getName()));
        }
        return column.getColumn();
    }

    private Long getParentId(D record) {
        try {
            return (Long) parentIdField.get(record);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("%s的@ChildrenParentId字段读取失败".formatted(doClass.getName()), e);
        }
    }

    private void setParentId(D record, Long parentId) {
        try {
            parentIdField.set(record, parentId);
        } catch (IllegalAccessException e) {
            throw new IllegalStateException("%s的@ChildrenParentId字段写入失败".formatted(doClass.getName()), e);
        }
    }

    private Long toParentId(Object value) {
        return switch (value) {
            case null -> null;
            case Long parentId -> parentId;
            case Number number -> number.longValue();
            case String text -> Long.valueOf(text);
            default -> throw new IllegalStateException("不支持的ParentId类型：%s".formatted(value.getClass().getName()));
        };
    }

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
        queryWrapper.eq(parentIdColumn, parentId);
        applyDefaultOrderBy(queryWrapper);
        return queryWrapper;
    }

    protected QueryWrapper<D> buildQueryWrapper(List<Long> parentIdList) {
        var queryWrapper = new QueryWrapper<D>();
        queryWrapper.in(parentIdColumn, parentIdList);
        applyDefaultOrderBy(queryWrapper);
        return queryWrapper;
    }

    protected QueryWrapper<D> buildQueryWrapper(Q query) {
        return queryWrapperBuilder.build(query);
    }
    // endregion


    @Override
    public List<Long> listParentId(Q query) {
        var queryWrapper = buildQueryWrapper(query);
        queryWrapper.select("DISTINCT " + parentIdColumn);
        return mapper.selectObjs(queryWrapper).stream()
                .map(this::toParentId)
                .toList();
    }

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
        return records.stream()
                .collect(Collectors.groupingBy(
                        this::getParentId,
                        Collectors.collectingAndThen(Collectors.toList(), entityMapper::doToDto)));
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
            var recordDo = entityMapper.dtoToDo(record);
            setParentId(recordDo, parentId);
            if (oldRecordsMap.containsKey(recordDo.getId())) {
                if (!(oldRecordsMap.get(recordDo.getId()).bizEqual(recordDo))) {
                    updateList.add(recordDo);
                }
                oldRecordsMap.remove(recordDo.getId());
            } else {
                insertList.add(recordDo);
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
