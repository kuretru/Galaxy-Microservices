package com.kuretru.api.common.util;

import com.github.dozermapper.core.Mapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Component
public class PojoUtils {

    private static Mapper beanConverter;

    /**
     * 消除unchecked警告
     *
     * @param object 源数据
     * @param <T>    目标数据
     * @return 消除警告后的目标数据
     */
    @SuppressWarnings("unchecked")
    public static <T> T cast(Object object) {
        return (T) object;
    }

    /**
     * 使用Dozer互相转换Pojo的泛型集合
     *
     * @param records 原类型List
     * @param tClass  目标类型Class
     * @param <S>     源类型
     * @param <T>     目标类型
     * @return 目标类型List
     */
    public static <S, T> List<T> map(List<S> records, Class<T> tClass) {
        return records.stream()
                .map(record -> beanConverter.map(record, tClass))
                .collect(Collectors.toList());
    }

    @Autowired
    public void setBeanConverter(Mapper beanConverter) {
        PojoUtils.beanConverter = beanConverter;
    }

}
