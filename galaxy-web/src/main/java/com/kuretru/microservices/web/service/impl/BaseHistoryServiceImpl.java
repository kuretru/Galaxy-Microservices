package com.kuretru.microservices.web.service.impl;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.microservices.web.entity.data.BaseHistoryDO;
import com.kuretru.microservices.web.entity.mapper.BaseHistoryEntityMapper;
import com.kuretru.microservices.web.entity.transfer.BaseHistoryDTO;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.service.BaseHistoryService;

import java.time.Instant;

/**
 * 历史数据型业务逻辑层基类，继承此类即可获得一些基础方法
 * 泛型：M->实体对应的数据访问层，D->实体对应的数据对象，T->实体对应的数据传输对象，Q->实体对应的查询对象
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseHistoryServiceImpl<M extends BaseMapper<D>, D extends BaseHistoryDO, T extends BaseHistoryDTO>
        implements BaseHistoryService<T> {

    protected final M mapper;
    protected final BaseHistoryEntityMapper<D, T> entityMapper;

    public BaseHistoryServiceImpl(M mapper, BaseHistoryEntityMapper<D, T> entityMapper) {
        this.mapper = mapper;
        this.entityMapper = entityMapper;
    }

    @Override
    public void save(T record) throws ServiceException {
        verifyDTO(record);
        D data = entityMapper.dtoToDo(record);
        data.setCreateTime(Instant.now());
        mapper.insert(data);
    }

    /**
     * 在业务层面验证传入的DTO内容是否合法
     *
     * @param record DTO
     * @throws ServiceException 不合法时抛出业务异常
     */
    protected void verifyDTO(T record) throws ServiceException {

    }

}
