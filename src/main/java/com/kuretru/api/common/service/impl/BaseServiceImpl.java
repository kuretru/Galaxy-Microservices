package com.kuretru.api.common.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.api.common.entity.data.BaseDO;
import com.kuretru.api.common.exception.ApiException;
import com.kuretru.api.common.exception.NotFoundException;
import com.kuretru.api.common.service.BaseService;
import org.springframework.beans.BeanUtils;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public abstract class BaseServiceImpl<M extends BaseMapper<D>, D extends BaseDO, T> implements BaseService<M, D, T> {

    protected M mapper;
    protected Class<D> doClass;
    protected Class<T> dtoClass;

    /**
     * 子类中必须调用此构造函数，传递参数
     * <pre>
     * @Service
     * public class DataOxfordServiceImpl extends BaseServiceImpl<DataOxfordMapper, DataOxfordDO, DataOxfordDTO> implements DataOxfordService {
     *
     *     @Autowired
     *     public DataOxfordServiceImpl(DataOxfordMapper mapper) {
     *         super(mapper, DataOxfordDO.class, DataOxfordDTO.class);
     *     }
     *
     * }
     * </pre>
     *
     * @param mapper   对应的Mapper
     * @param doClass  对应DO的class
     * @param dtoClass 对应DTO的class
     */
    public BaseServiceImpl(M mapper, Class<D> doClass, Class<T> dtoClass) {
        this.mapper = mapper;
        this.doClass = doClass;
        this.dtoClass = dtoClass;
    }

    @Override
    public T get(Long id) {
        D record = mapper.selectById(id);
        return doToDTO(record);
    }

    @Override
    public List<T> list() {
        QueryWrapper<D> queryWrapper = new QueryWrapper<>();
        Field[] fields = doClass.getDeclaredFields();
        for (Field field : fields) {
            if ("sequence".equals(field.getName())) {
                queryWrapper.orderByAsc("sequence");
                break;
            }
        }
        List<D> records = mapper.selectList(queryWrapper);
        return doToDTO(records);
    }

    @Override
    public int count() {
        Integer result = mapper.selectCount(null);
        return null == result ? 0 : result;
    }

    @Override
    public T save(T record) throws ApiException {
        D data = dtoToDO(record);
        data.addCrateTime();
        mapper.insert(data);
        return get(data.getId());
    }

    @Override
    public T update(T record) throws ApiException {
        D data = dtoToDO(record);
        int rows = mapper.updateById(data);
        if (1 != rows) {
            throw new NotFoundException("无此记录！");
        }
        return get(data.getId());
    }

    @Override
    public int remove(Long id) throws ApiException {
        int rows = mapper.deleteById(id);
        if (1 != rows) {
            throw new NotFoundException("无此记录！");
        }
        return rows;
    }

    @Override
    public T doToDTO(D record) {
        if (record == null) {
            return null;
        }
        T result = netInstance(dtoClass);
        BeanUtils.copyProperties(record, result);
        return result;
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
    public D dtoToDO(T record) {
        if (record == null) {
            return null;
        }
        D result = netInstance(doClass);
        BeanUtils.copyProperties(record, result);
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

    private <K> K netInstance(Class<K> kClass) {
        try {
            return kClass.newInstance();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
        throw new Error("目标类缺少无参构造函数");
    }

}
