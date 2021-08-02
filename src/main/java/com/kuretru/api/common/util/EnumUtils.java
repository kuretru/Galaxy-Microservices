package com.kuretru.api.common.util;

import com.kuretru.api.common.entity.enums.BaseEnum;

import java.util.Map;
import java.util.TreeMap;

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
    public static <E extends BaseEnum<E>> E valueOf(Class<E> eClass, Short code) {
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
    public static <E extends BaseEnum<E>> E valueOf(Class<E> eClass, String value) {
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

    /**
     * 将枚举转换成键值对的形式
     *
     * @param values 枚举
     * @param <E>    派生自BaseEnum的枚举类
     * @return 键值对形式的枚举
     */
    public static <E extends BaseEnum<E>> Map<BaseEnum<E>, String> buildEnumMap(BaseEnum<E>[] values) {
        Map<BaseEnum<E>, String> result = new TreeMap<>();
        for (BaseEnum<E> value : values) {
            result.put(value, value.getValue());
        }
        return result;
    }

}
