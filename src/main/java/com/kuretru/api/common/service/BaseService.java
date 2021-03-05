package com.kuretru.api.common.service;

import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ServiceException;

import java.util.List;
import java.util.UUID;

/**
 * 业务逻辑层基类接口
 * 泛型：D->实体对应的数据对象，T->实体对应的数据传输对象，M->实体对应的数据访问层
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseService<T extends BaseDTO> {

    /**
     * 根据物理主键查询一条记录
     *
     * @param id 物理主键ID
     * @return 一条记录
     */
    T get(Long id);

    /**
     * 根据业务逻辑主键查询一条记录
     *
     * @param uuid 业务逻辑主键UUID
     * @return 一条记录
     */
    T get(UUID uuid);

    /**
     * 查询所有记录
     *
     * @return 所有记录
     */
    List<T> list();

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
     */
    T save(T record);

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
