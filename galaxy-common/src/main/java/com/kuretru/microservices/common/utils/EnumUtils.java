package com.kuretru.microservices.common.utils;

import com.kuretru.microservices.common.entity.enums.BaseEnum;
import com.kuretru.microservices.common.entity.enums.EnumDTO;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
        Map<BaseEnum<E>, String> result = new HashMap<>(HashMapUtils.initialCapacity(values.length));
        for (BaseEnum<E> value : values) {
            result.put(value, value.getValue());
        }
        return result;
    }

    /**
     * 将枚举转换成字符串键值对的形式
     * <p>
     * 键->枚举名称，值->枚举内容
     * <p>
     * {
     * "MALE": "男",
     * "FEMALE":"女"
     * }
     *
     * @param values 枚举
     * @param <E>    派生自BaseEnum的枚举类
     * @return 字符串键值对形式的枚举
     */
    public static <E extends BaseEnum<E>> Map<String, String> buildStringEnumMap(BaseEnum<E>[] values) {
        Map<String, String> result = new HashMap<>(HashMapUtils.initialCapacity(values.length));
        for (BaseEnum<E> value : values) {
            result.put(value.name(), value.getValue());
        }
        return result;
    }

    /**
     * 将枚举转换成供前端使用的DTO
     * <p>
     * [
     * {"label":"MALE","value":"男"},
     * {"label":"FEMALE","value":"女"},
     * ]
     *
     * @param values 举
     * @param <E>    派生自BaseEnum的枚举类
     * @return 前端使用的DTO
     */
    public static <E extends BaseEnum<E>> List<EnumDTO> buildDTO(BaseEnum<E>[] values) {
        List<EnumDTO> result = new ArrayList<>();
        for (BaseEnum<E> value : values) {
            result.add(new EnumDTO(value.name(), value.getValue()));
        }
        return result;
    }

}
