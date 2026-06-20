package com.kuretru.microservices.web.v2.service;

import com.kuretru.microservices.web.entity.PaginationQuery;
import com.kuretru.microservices.web.entity.PaginationResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;

import java.util.List;

public interface BaseService<T extends BaseDTO, Q> {

    /**
     * 根据物理主键查询一条记录
     *
     * @param id 物理主键ID
     * @return 一条记录，找不到时返回Null
     * @throws ServiceException 没有权限时，抛出业务异常
     */
    T get(Long id) throws ServiceException;

    /**
     * 根据查询条件，查询所有主键ID
     *
     * @param query 查询条件
     * @return 所有ID
     * @throws ServiceException 没有权限时，抛出业务异常
     */
    List<Long> listId(Q query) throws ServiceException;

    /**
     * 根据查询条件，查询所有记录
     *
     * @param query 查询条件
     * @return 所有记录，找不到时返回空List
     * @throws ServiceException 没有权限时，抛出业务异常
     */
    List<T> list(Q query) throws ServiceException;

    /**
     * 根据查询条件，分页查询所有记录
     *
     * @param pagination 分页参数
     * @param query      查询条件
     * @return 符合查询条件，分页后的所有记录
     * @throws ServiceException 没有权限时，抛出业务异常
     */
    PaginationResponse<T> list(PaginationQuery pagination, Q query) throws ServiceException;

    /**
     * 保存新记录
     *
     * @param record 新记录
     * @return 保存后的新记录
     * @throws ServiceException 校验数据失败时会抛出业务异常
     */
    T save(T record) throws ServiceException;

    /**
     * 更新记录，必须传入所有字段
     *
     * @param record 包含新数据及其他所有字段的记录
     * @return 更新后的新记录
     * @throws ServiceException 找不到指定记录或校验数据失败时会抛出业务异常
     */
    T update(T record) throws ServiceException;

    /**
     * 根据业务逻辑主键删除一条记录
     *
     * @param id 物理主键ID
     * @throws ServiceException 找不到指定记录时会抛出业务异常
     */
    void remove(Long id) throws ServiceException;

}
