package com.kuretru.api.common.entity.enums;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseEnum {

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
