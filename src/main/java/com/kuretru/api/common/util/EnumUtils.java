package com.kuretru.api.common.util;

import com.kuretru.api.common.entity.enums.BaseEnum;

/**
 * 枚举相关工具类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class EnumUtils {

    private EnumUtils() {

    }

    /**
     * 从枚举编号还原枚举
     *
     * @param eClass 枚举实体的Class
     * @param code   枚举编号
     * @param <E>    派生于BaseEnum的枚举类型
     * @return 枚举实体
     */
    public static <E extends BaseEnum> E valueOf(Class<E> eClass, Short code) {
        if (code == null) {
            throw new NullPointerException("枚举编号为null");
        }
        for (E element : eClass.getEnumConstants()) {
            if (element.getCode() == code) {
                return element;
            }
        }
        throw new IllegalArgumentException("不存在枚举编号所对应的枚举实体");
    }

    /**
     * 从枚举内容还原枚举
     *
     * @param eClass 枚举实体的Class
     * @param value  枚举内容
     * @param <E>    派生于BaseEnum的枚举类型
     * @return 枚举实体
     */
    public static <E extends BaseEnum> E valueOf(Class<E> eClass, String value) {
        if (value == null) {
            throw new NullPointerException("枚举内容为null");
        }
        for (E element : eClass.getEnumConstants()) {
            if (element.getValue().equals(value)) {
                return element;
            }
        }
        throw new IllegalArgumentException("不存在枚举内容所对应的枚举实体");
    }

}
