package com.kuretru.microservices.web.v2.service;

import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;

import java.util.List;
import java.util.Map;

public interface BaseOneToManyService<T extends BaseDTO> {

    /**
     * 根据主表ID查询所有记录
     *
     * @param parentId 主表ID
     * @return 主表下所有记录
     */
    List<T> listByParentId(Long parentId);

    /**
     * 根据主表ID查询所有记录
     *
     * @param parentIdList 主表ID列表
     * @return 主表下所有记录
     */
    Map<Long, List<T>> listByParentId(List<Long> parentIdList);

    /**
     * 同步所有变更，无则新增，有则修改，不需要则删除
     *
     * @param parentId   主表ID
     * @param newRecords 新记录
     * @return 修改后的所有记录
     */
    List<T> syncByParentId(Long parentId, List<T> newRecords);

    /**
     * 根据主表ID全部删除
     *
     * @param parentId 主表ID
     */
    void removeByParentId(Long parentId);

}
