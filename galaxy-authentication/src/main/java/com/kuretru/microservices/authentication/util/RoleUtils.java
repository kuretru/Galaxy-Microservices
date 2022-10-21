package com.kuretru.microservices.authentication.util;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * 角色相关的静态工具类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class RoleUtils {

    private RoleUtils() {

    }

    /**
     * 判断用户是否含有给定的任意角色
     *
     * @param expected 用户实际拥有的角色
     * @param actual   传入要判断的角色
     * @return 是否含有指定角色
     */
    public static boolean hasRole(Set<String> expected, String actual) {
        return expected.contains(actual);
    }

    /**
     * 判断用户是否含有给定的任意角色
     *
     * @param expected 用户实际拥有的角色
     * @param actual   传入要判断的角色
     * @return 是否含有指定角色
     */
    public static boolean hasRole(Set<String> expected, String[] actual) {
        for (String role : actual) {
            if (expected.contains(role)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 判断用户是否含有给定的所有角色
     *
     * @param expected 用户实际拥有的角色
     * @param actual   传入要判断的角色
     * @return 是否含有指定角色
     */
    public static boolean hasRoles(Set<String> expected, String actual) {
        return expected.contains(actual);
    }

    /**
     * 判断用户是否含有给定的所有角色
     *
     * @param expected 用户实际拥有的角色
     * @param actual   传入要判断的角色
     * @return 是否含有指定角色
     */
    public static boolean hasRoles(Set<String> expected, String[] actual) {
        Set<String> set = new HashSet<>(Arrays.asList(actual));
        for (String role : expected) {
            if (!set.contains(role)) {
                return false;
            }
        }
        return true;
    }

}
