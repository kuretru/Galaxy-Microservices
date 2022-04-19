package com.kuretru.microservices.common.storage;

import java.time.Duration;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 基于访问计数器的的内存存储器
 * 访问指定次数后，执行一次回收过期键操作
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class InMemoryCounterStorage<K, V> implements InMemoryStorage<K, V> {

    private final ConcurrentMap<K, Value<V>> map;
    private final int purgeInterval;
    private final AtomicInteger visitCount;

    public InMemoryCounterStorage(int purgeInterval) {
        this.map = new ConcurrentHashMap<>(16);
        this.purgeInterval = purgeInterval;
        this.visitCount = new AtomicInteger(0);
    }

    @Override
    public boolean hasKey(K key) {
        return exist(key) != null;
    }

    @Override
    public V get(K key) {
        Value<V> value = exist(key);
        if (value == null) {
            return null;
        }

        if (visitCount.getAndIncrement() >= purgeInterval) {
            synchronized (visitCount) {
                if (visitCount.get() > purgeInterval) {
                    visitCount.set(0);
                    purgeExpiredKeys();
                }
            }
        }

        return value.data;
    }

    @Override
    public V getAndExpire(K key, Duration duration) {
        Value<V> value = exist(key);
        if (value == null) {
            return null;
        }
        value.expireTime = System.currentTimeMillis() + duration.toMillis();
        return value.data;
    }

    @Override
    public V getAndDelete(K key) {
        Value<V> value = exist(key);
        if (value == null) {
            return null;
        }
        map.remove(key);
        return value.data;
    }

    @Override
    public boolean expire(K key, Duration duration) {
        Value<V> value = exist(key);
        if (value == null) {
            return false;
        }
        value.expireTime = System.currentTimeMillis() + duration.toMillis();
        return true;
    }

    @Override
    public void set(K key, V value, Duration duration) {
        Value<V> record = new Value<>(value, duration.toMillis());
        map.put(key, record);
    }

    @Override
    public boolean delete(K key) {
        Value<V> value = map.remove(key);
        return value != null;
    }

    @Override
    public void purgeExpiredKeys() {
        long now = System.currentTimeMillis();
        for (Map.Entry<K, Value<V>> entry : map.entrySet()) {
            if (now > entry.getValue().expireTime) {
                map.remove(entry.getKey());
            }
        }
    }

    @Override
    public void clear() {
        map.clear();
        visitCount.set(0);
    }

    private Value<V> exist(K key) {
        Value<V> value = map.get(key);
        if (value == null) {
            return null;
        } else if (System.currentTimeMillis() > value.expireTime) {
            map.remove(key);
            return null;
        }
        return value;
    }

    private static class Value<V> {

        private final V data;
        private long expireTime;

        public Value(V data, long expireMillis) {
            this.data = data;
            this.expireTime = System.currentTimeMillis() + expireMillis;
        }

    }

}
