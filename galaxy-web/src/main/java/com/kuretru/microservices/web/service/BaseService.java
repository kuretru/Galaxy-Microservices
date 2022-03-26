package com.kuretru.microservices.web.service;

import com.kuretru.microservices.web.entity.PaginationQuery;
import com.kuretru.microservices.web.entity.PaginationResponse;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.exception.ServiceException;

import java.util.List;
import java.util.UUID;

/**
 * 业务逻辑层基类接口
 * 泛型：T->实体对应的数据传输对象，Q->实体对应的查询对象
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseService<T extends BaseDTO, Q> {

    /**
     * 根据物理主键查询一条记录
     *
     * @param id 物理主键ID
     * @return 一条记录，找不到时返回Null
     */
    T get(Long id);

    /**
     * 根据业务逻辑主键查询一条记录
     *
     * @param uuid 业务逻辑主键UUID
     * @return 一条记录，找不到时返回Null
     */
    T get(UUID uuid);

    /**
     * 查询所有记录
     *
     * @return 所有记录，找不到时返回空List
     */
    List<T> list();

    /**
     * 根据查询条件，查询所有记录
     *
     * @param query 查询条件
     * @return 所有记录，找不到时返回空List
     */
    List<T> list(Q query);

    /**
     * 分页查询所有记录
     *
     * @param paginationQuery 分页参数
     * @return 分页后的所有记录
     */
    PaginationResponse<T> list(PaginationQuery paginationQuery);

    /**
     * 根据查询条件，分页查询所有记录
     *
     * @param pagination 分页参数
     * @param query      查询条件
     * @return 符合查询条件，分页后的所有记录
     */
    PaginationResponse<T> list(PaginationQuery pagination, Q query);

    /**
     * 查询记录条数
     *
     * @return 记录条数
     */
    int count();

    /**
     * 保存新记录
     *
     * @param record 新记录
     * @return 保存后的新记录
     * @throws ServiceException 校验数据失败时会引发异常
     */
    T save(T record) throws ServiceException;

    /**
     * 更新记录，必须传入所有字段
     *
     * @param record 包含新数据及其他所有字段的记录
     * @return 更新后的新记录
     * @throws ServiceException 找不到指定记录时会引发NotFound异常
     */
    T update(T record) throws ServiceException;

    /**
     * 根据业务逻辑主键删除一条记录
     *
     * @param uuid 业务逻辑主键UUID
     * @throws ServiceException 找不到指定记录时会引发NotFound异常
     */
    void remove(UUID uuid) throws ServiceException;

}
