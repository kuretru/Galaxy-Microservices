package com.kuretru.api.common.service;

import java.util.List;

/**
 * 基本业务逻辑接口
 * 泛型：V表示该实体对应的VO，T表示该实体对应的DTO
 *
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public interface BaseService<V, T> {

    /**
     * 根据主键ID获取记录
     *
     * @param id 主键ID
     * @return 记录VO
     */
    V get(Integer id);

    /**
     * 获取所有记录
     *
     * @return 所有记录VO
     */
    List<V> list();

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
     * @return 新纪录VO
     */
    V save(T record);

    /**
     * 根据主键ID删除记录
     *
     * @param id 主键ID
     * @return
     */
    int remove(Integer id);

    /**
     * 更新记录
     *
     * @param record 新数据
     * @return 新数据VO
     */
    V update(T record);

}
