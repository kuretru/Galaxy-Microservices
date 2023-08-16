package com.kuretru.microservices.web.manager;

import com.kuretru.microservices.web.manager.impl.TraceIdManagerImpl;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@SpringBootTest
class TraceIdManagerTest {

    private static final int SEQUENCE_COUNT = TraceIdManagerImpl.MAX_SEQUENCE - TraceIdManagerImpl.MIN_SEQUENCE;

    private final TraceIdManager manager;

    @Autowired
    public TraceIdManagerTest(TraceIdManager manager) {
        this.manager = manager;
    }

    @Test
    void generateTraceId() throws InterruptedException {
        Set<Integer> sequenceSet = new HashSet<>();
        for (int i = 0; i < SEQUENCE_COUNT / 100; i++) {
            Thread thread = new Thread(() -> {
                for (int j = 0; j < 100; j++) {
                    synchronized (sequenceSet) {
                        sequenceSet.add(getSequence());
                    }
                }
            });
            thread.start();
            thread.join();
        }
        assertEquals(SEQUENCE_COUNT, sequenceSet.size());
        assertTrue(sequenceSet.contains(getSequence()));
        assertEquals(36, manager.generateTraceId().length());
    }

    private int getSequence() {
        String traceId = manager.generateTraceId();
        int index = traceId.lastIndexOf("_");
        return Integer.parseInt(traceId.substring(index + 1));
    }

}
