package com.kuretru.microservices.common.entity.enums;

/**
 * 业务枚举基类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseEnum<T> extends Comparable<T> {

    /**
     * 获取枚举编号
     *
     * @return 枚举编号
     */
    short getCode();

    /**
     * 获取枚举内容
     *
     * @return 枚举内容
     */
    String getValue();

    /**
     * Java enum原生的获取枚举名称的方法
     *
     * @return 枚举名称
     */
    String name();

}
