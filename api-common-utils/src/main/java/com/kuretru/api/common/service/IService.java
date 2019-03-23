package com.kuretru.api.common.service;

import java.util.List;

/**
 * 基本业务逻辑接口
 * 泛型：T表示该实体对应的DTO，D表示该实体对应的DO
 *
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public interface IService<T> {

    /**
     * 根据主键ID获取记录
     *
     * @param id 主键ID
     * @return 记录DTO
     */
    T get(Long id);

    /**
     * 获取所有记录
     *
     * @return 所有记录DTO
     */
    List<T> list();

    /**
     * 统计记录条数
     *
     * @return 记录条数
     */
    int count();

    /**
     * 保存新记录
     *
     * @param record 新纪录
     * @return 新纪录DTO
     */
    T save(T record);

    /**
     * 根据主键ID删除记录
     *
     * @param id 主键ID
     * @return 受影响的行数
     */
    int remove(Long id);

    /**
     * 更新记录
     *
     * @param record 新数据
     * @return 新数据DTO
     */
    T update(T record);

}
