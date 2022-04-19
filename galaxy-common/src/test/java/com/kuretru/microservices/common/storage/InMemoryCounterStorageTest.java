package com.kuretru.microservices.common.storage;

import lombok.SneakyThrows;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.time.Duration;
import java.util.concurrent.ConcurrentMap;

import static org.junit.jupiter.api.Assertions.*;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
class InMemoryCounterStorageTest {

    private static final int PURGE_INTERVAL = 10;
    private final InMemoryStorage<String, String> inMemoryStorage;

    public InMemoryCounterStorageTest() {
        this.inMemoryStorage = new InMemoryCounterStorage<>(PURGE_INTERVAL);
    }

    @Test
    @SneakyThrows
    void autoPurge() {
        inMemoryStorage.clear();
        inMemoryStorage.set("autoPurge", "autoPurge", Duration.ofMillis(500));
        Thread.sleep(1000);

        ConcurrentMap<String, ?> map = getInnerMap();

        inMemoryStorage.set("autoPurge1", "autoPurge", Duration.ofMinutes(1));
        assertEquals(2, map.size());
        for (int i = 0; i < PURGE_INTERVAL; i++) {
            inMemoryStorage.get("autoPurge1");
        }
        assertEquals(2, map.size());
        inMemoryStorage.get("autoPurge1");
        assertEquals(1, map.size());
    }

    @Test
    void hasKey() {
        assertFalse(inMemoryStorage.hasKey("hasKey"));
        inMemoryStorage.set("hasKey", "hasKey", Duration.ofMinutes(1));
        assertTrue(inMemoryStorage.hasKey("hasKey"));
    }

    @Test
    void get() {
        assertNull(inMemoryStorage.get("get"));
        inMemoryStorage.set("get", "get", Duration.ofMinutes(1));
        assertEquals("get", inMemoryStorage.get("get"));
    }

    @Test
    @SneakyThrows
    void getAndExpire() {
        assertNull(inMemoryStorage.getAndExpire("getAndExpire", Duration.ofMinutes(1)));
        inMemoryStorage.set("getAndExpire", "getAndExpire", Duration.ofMillis(500));
        assertEquals("getAndExpire", inMemoryStorage.getAndExpire("getAndExpire", Duration.ofMinutes(1)));
        Thread.sleep(1000);
        assertEquals("getAndExpire", inMemoryStorage.get("getAndExpire"));
    }

    @Test
    void getAndDelete() {
        assertNull(inMemoryStorage.getAndDelete("getAndDelete"));
        inMemoryStorage.set("getAndDelete", "getAndDelete", Duration.ofMinutes(1));
        assertEquals("getAndDelete", inMemoryStorage.getAndDelete("getAndDelete"));
        assertNull(inMemoryStorage.get("getAndDelete"));
    }

    @Test
    @SneakyThrows
    void expire() {
        assertFalse(inMemoryStorage.expire("expire", Duration.ofMinutes(1)));
        inMemoryStorage.set("expire", "expire", Duration.ofMillis(500));
        assertEquals("expire", inMemoryStorage.get("expire"));
        assertTrue(inMemoryStorage.expire("expire", Duration.ofMinutes(1)));
        Thread.sleep(1000);
        assertEquals("expire", inMemoryStorage.get("expire"));
    }

    @Test
    void set() {
        assertNull(inMemoryStorage.get("set"));
        inMemoryStorage.set("set", "set", Duration.ofMinutes(1));
        assertEquals("set", inMemoryStorage.get("set"));
    }

    @Test
    void delete() {
        assertFalse(inMemoryStorage.delete("delete"));
        inMemoryStorage.set("delete", "delete", Duration.ofMinutes(1));
        assertTrue(inMemoryStorage.delete("delete"));
    }

    @Test
    @SneakyThrows
    void purgeExpiredKey() {
        inMemoryStorage.set("purgeExpiredKey", "purgeExpiredKey", Duration.ofMillis(500));
        assertEquals("purgeExpiredKey", inMemoryStorage.get("purgeExpiredKey"));
        inMemoryStorage.purgeExpiredKeys();
        assertEquals("purgeExpiredKey", inMemoryStorage.get("purgeExpiredKey"));
        Thread.sleep(1000);
        inMemoryStorage.purgeExpiredKeys();

        ConcurrentMap<String, ?> map = getInnerMap();
        assertFalse(map.containsKey("purgeExpiredKey"));
        assertNull(inMemoryStorage.get("purgeExpiredKey"));
    }

    @Test
    void clear() {
        inMemoryStorage.set("clear", "clear", Duration.ofMinutes(1));
        assertEquals("clear", inMemoryStorage.get("clear"));
        inMemoryStorage.clear();
        assertNull(inMemoryStorage.get("clear"));
    }

    @SneakyThrows
    private ConcurrentMap<String, ?> getInnerMap() {
        Class<?> clazz = inMemoryStorage.getClass();
        Field field = clazz.getDeclaredField("map");
        field.setAccessible(true);
        return (ConcurrentMap<String, ?>)field.get(inMemoryStorage);
    }

}
