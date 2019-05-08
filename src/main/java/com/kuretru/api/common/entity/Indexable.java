package com.kuretru.api.common.entity;

/**
 * 对于属性中有Long类型的ID字段的POJO，可以实现该接口
 */
public interface Indexable {

    /**
     * 获取主键
     *
     * @return 主键
     */
    Long getId();

}
