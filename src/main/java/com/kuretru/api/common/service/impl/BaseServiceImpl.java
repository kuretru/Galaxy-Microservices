package com.kuretru.api.common.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.api.common.constant.ServiceErrorCodes;
import com.kuretru.api.common.constant.UserErrorCodes;
import com.kuretru.api.common.entity.data.BaseDO;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;
import com.kuretru.api.common.service.BaseService;
import lombok.SneakyThrows;
import org.springframework.beans.BeanUtils;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * 业务逻辑层基类，继承此类即可获得一些基础方法
 * 泛型：M->实体对应的数据访问层，D->实体对应的数据对象，T->实体对应的数据传输对象
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseServiceImpl<M extends BaseMapper<D>, D extends BaseDO, T extends BaseDTO> implements BaseService<T> {

    protected final M mapper;
    protected final Class<D> doClass;
    protected final Class<T> dtoClass;

    protected BaseServiceImpl(M mapper, Class<D> doClass, Class<T> dtoClass) {
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

    @Override
    public List<T> list() {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.orderByAsc("id");
        List<D> records = mapper.selectList(queryWrapper);
        return doToDto(records);
    }

    @Override
    public int count() {
        Integer result = mapper.selectCount(null);
        return null == result ? 0 : result;
    }

    @Override
    public T save(T record) {
        D data = dtoToDo(record);
        data.setUuid(UUID.randomUUID().toString());
        Instant now = Instant.now();
        data.setCreateTime(now);
        data.setUpdateTime(now);
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
            throw new ServiceException.NotFound(UserErrorCodes.REQUEST_PARAMETER_ERROR, "该记录ID不存在");
        } else if (1 != rows) {
            throw new ServiceException.InternalServerError(ServiceErrorCodes.SYSTEM_EXECUTION_ERROR, "发现多个相同业务主键");
        }
        return get(data.getId());
    }

    @Override
    public void remove(UUID uuid) throws ServiceException {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("uuid", uuid.toString());
        int rows = mapper.delete(queryWrapper);
        if (0 == rows) {
            throw new ServiceException.NotFound(UserErrorCodes.REQUEST_PARAMETER_ERROR, "该记录ID不存在");
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
    @SneakyThrows
    protected T doToDto(D record) {
        if (record == null) {
            return null;
        }
        T result = dtoClass.getConstructor().newInstance();
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
    @SneakyThrows()
    protected D dtoToDo(T record) {
        if (record == null) {
            return null;
        }
        D result = doClass.getConstructor().newInstance();
        BeanUtils.copyProperties(record, result);
        result.setUuid(record.getId().toString());
        return result;
    }

    /**
     * 将数据传输实体批量转换为数据实体
     *
     * @param records 数据传输实体列表
     * @return 数据实体列表
     */
    protected List<D> dtoToDo(List<T> records) {
        List<D> result = new ArrayList<>(records.size());
        for (T record : records) {
            result.add(dtoToDo(record));
        }
        return result;
    }

}
