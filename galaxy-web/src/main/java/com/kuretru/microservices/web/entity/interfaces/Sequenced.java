package com.kuretru.microservices.web.entity.interfaces;

public interface Sequenced {

    /** 获取排序标识 */
    Integer getSequence();

    /** 设置排序标识 */
    void setSequence(Integer sequence);

}
