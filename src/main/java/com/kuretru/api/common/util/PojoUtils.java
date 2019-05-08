//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by Fernflower decompiler)
//

package com.kuretru.api.common.util;

import com.kuretru.api.common.entity.Indexable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class PojoUtils {

    /**
     * 将记录列表以Id为key转换为Hash表
     *
     * @param records 记录列表
     * @param <T>     记录类型
     * @return Hash表
     */
    public static <T extends Indexable> Map<Long, T> toHashMap(List<T> records) {
        HashMap<Long, T> result = new HashMap<>((int) (records.size() / 0.75 + 1));
        for (T record : records) {
            result.put(record.getId(), record);
        }
        return result;
    }

}
