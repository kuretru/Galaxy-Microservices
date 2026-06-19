package com.kuretru.microservices.web.v2.service;

import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.v2.service.ability.children.ChildrenOperator;

import java.util.List;
import java.util.Map;

public interface ChildrenCapable<T extends BaseDTO> {

    ChildrenOperator<T> childrenOperator();

    /**
     * 根据主表ID查询所有记录
     *
     * @param parentId 主表ID
     * @return 主表下所有记录
     */
    default List<T> listByParentId(Long parentId) {
        return childrenOperator().listByParentId(parentId);
    }

    /**
     * 根据主表ID查询所有记录
     *
     * @param parentIdList 主表ID列表
     * @return 主表下所有记录
     */
    default Map<Long, List<T>> listByParentId(List<Long> parentIdList) {
        return childrenOperator().listByParentId(parentIdList);
    }

    /**
     * 同步所有变更，无则新增，有则修改，不需要则删除
     *
     * @param parentId   主表ID
     * @param newRecords 新记录
     * @return 修改后的所有记录
     */
    default List<T> syncByParentId(Long parentId, List<T> newRecords) {
        return childrenOperator().syncByParentId(parentId, newRecords);
    }

    /**
     * 根据主表ID全部删除
     *
     * @param parentId 主表ID
     */
    default void removeByParentId(Long parentId) {
        childrenOperator().removeByParentId(parentId);
    }

}
