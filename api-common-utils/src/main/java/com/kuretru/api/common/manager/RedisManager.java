package com.kuretru.api.common.manager;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public interface RedisManager {

    /**
     * 查询Redis操作
     *
     * @param key 键
     * @return 值
     */
    String get(String key);

    /**
     * 新增Redis操作
     *
     * @param key   键
     * @param value 值
     */
    void create(String key, String value);

    /**
     * 更新Redis操作(延期)
     *
     * @param key 键
     */
    void update(String key);

    /**
     * 删除Redis操作
     *
     * @param key 键
     */
    void remove(String key);


}
