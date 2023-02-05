package com.kuretru.microservices.web.service;

import com.kuretru.microservices.web.entity.transfer.BaseHistoryDTO;
import com.kuretru.microservices.web.exception.ServiceException;

/**
 * 历史数据型业务逻辑层基类接口
 * 泛型：T->实体对应的数据传输对象
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseHistoryService<T extends BaseHistoryDTO> {

    /**
     * 保存新记录
     *
     * @param record 新记录
     * @throws ServiceException 校验数据失败时会引发异常
     */
    void save(T record) throws ServiceException;

}
