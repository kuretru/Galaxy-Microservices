package com.kuretru.api.common.service;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.api.common.entity.data.BaseDO;

import java.util.List;

/**
 * 基本业务逻辑接口，基本表直接继承此接口，并实现转换方法即可直接使用
 * 泛型：T表示该实体对应的DTO，D表示该实体对应的DO
 *
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public interface BaseService<M extends BaseMapper<D>, D extends BaseDO, T> {

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

    /**
     * 设置通用Mapper方法
     * TODO 待寻找通用解决方案
     *
     * @param mapper 对应实体类的通用Mapper
     */
    void setMapper(M mapper);

    /**
     * 将DO转换为DTO的方法
     * TODO 待寻找通用解决方案
     *
     * @param record DO记录
     * @return DTO记录
     */
    T doToDTO(D record);

    /**
     * 批量将DO转换为DTO的方法
     *
     * @param records DO记录集
     * @return DTO记录集
     */
    List<T> doToDTO(List<D> records);

    /**
     * 将DTO转换为DO的方法
     * TODO 待寻找通用解决方案
     *
     * @param record DTO记录
     * @return DO记录
     */
    D dtoToDO(T record);

    /**
     * 批量将DTO转换为DO的方法
     *
     * @param records DTO记录集
     * @return DO记录集
     */
    List<D> dtoToDO(List<T> records);

}
