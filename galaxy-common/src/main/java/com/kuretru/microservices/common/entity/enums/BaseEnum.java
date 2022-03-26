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

}
