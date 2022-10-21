package com.kuretru.microservices.common.utils;

import com.kuretru.microservices.common.constant.EmptyConstants;

import java.util.UUID;

/**
 * UUID相关的静态工具类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class UuidUtils {

    private UuidUtils() {

    }

    /**
     * 判断UUID是否为空的
     *
     * @param uuid uuid
     * @return 是空的
     */
    public static boolean isEmpty(UUID uuid) {
        return uuid == null || EmptyConstants.EMPTY_UUID.equals(uuid);
    }

    /**
     * 判断UUID是否为非空的
     *
     * @param uuid uuid
     * @return 非空的
     */
    public static boolean isNotEmpty(UUID uuid) {
        return !isEmpty(uuid);
    }

}
