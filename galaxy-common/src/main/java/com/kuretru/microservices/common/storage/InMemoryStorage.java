package com.kuretru.microservices.common.storage;

import java.time.Duration;

/**
 * 简易的内存存储器
 *
 * @param <K> 键
 * @param <V> 值
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface InMemoryStorage<K, V> {

    /**
     * 判断数据库中是否包含指定键
     *
     * @param key 指定键
     * @return 是否包含
     */
    boolean hasKey(K key);

    /**
     * 根据指定键从数据库中获取值
     *
     * @param key 指定键
     * @return 对应值
     */
    V get(K key);

    /**
     * 根据指定键从数据库中获取值，并延长过期时间
     *
     * @param key      指定键
     * @param duration 延长时间
     * @return 对应值
     */
    V getAndExpire(K key, Duration duration);

    /**
     * 根据指定键从数据库中获取值，并删除键
     *
     * @param key 指定键
     * @return 对应值
     */
    V getAndDelete(K key);

    /**
     * 延长键的过期时间，如果不存在则不延长
     *
     * @param key      指定键
     * @param duration 延长时间
     * @return 是否成功
     */
    boolean expire(K key, Duration duration);

    /**
     * 设置键值对
     *
     * @param key      键
     * @param value    值
     * @param duration 过期时间
     */
    void set(K key, V value, Duration duration);

    /**
     * 删除指定键，如果不存在则不删除
     *
     * @param key 指定键
     * @return 是否成功
     */
    boolean delete(K key);

    /**
     * 清理所有过期键
     */
    void purgeExpiredKeys();

    /**
     * 清理所有键
     */
    void clear();

}
