package com.kuretru.microservices.common.utils;

/**
 * HashMap相关工具类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class HashMapUtils {

    private static final int DEFAULT_INITIAL_CAPACITY = 16;

    private HashMapUtils() {

    }

    /**
     * 计算Hash集合的初始化大小
     *
     * @param size 需要存储的元素个数
     * @return 初始化大小
     */
    public static int initialCapacity(int size) {
        return initialCapacity(size, 0.75);
    }

    /**
     * 计算Hash集合的初始化大小
     *
     * @param size         需要存储的元素个数
     * @param loaderFactor 负载因子
     * @return 初始化大小
     */
    public static int initialCapacity(int size, double loaderFactor) {
        if (size < (int)(DEFAULT_INITIAL_CAPACITY * loaderFactor)) {
            return DEFAULT_INITIAL_CAPACITY;
        }
        return (int)(size / loaderFactor) + 1;
    }

}
