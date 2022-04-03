package com.kuretru.microservices.oauth2.server.memory;

/**
 * 管理用户请求OAuth2认证时，每个请求必须保证其State字段唯一
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface OAuth2UniqueStateMemory {

    /**
     * 验证该State字段是否被使用过
     *
     * @param state State字段
     * @return 是否被使用过
     */
    boolean exist(String state);

}
