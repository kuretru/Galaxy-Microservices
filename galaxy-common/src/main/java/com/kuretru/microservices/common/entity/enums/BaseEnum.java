package com.kuretru.microservices.common.entity.enums;

/**
 * 业务枚举基类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseEnum<T> extends Comparable<T> {

    /**
     * 获取枚举值
     *
     * @return 枚举值
     */
    String getValue();

    /**
     * 获取枚举标签
     *
     * @return 枚举标签
     */
    String getLabel();

    /**
     * Java enum原生的获取枚举名称的方法
     *
     * @return 枚举名称
     */
    String name();

}
