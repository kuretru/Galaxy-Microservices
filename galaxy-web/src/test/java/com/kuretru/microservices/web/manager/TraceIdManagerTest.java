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
        for (int i = 0; i < SEQUENCE_COUNT; i++) {
            Thread thread = new Thread(() -> {
                synchronized (sequenceSet) {
                    sequenceSet.add(getSequence());
                }
            });
            thread.start();
            thread.join();
        }
        assertEquals(SEQUENCE_COUNT, sequenceSet.size());
        assertTrue(sequenceSet.contains(getSequence()));
    }

    private int getSequence() {
        String traceId = manager.generateTraceId();
        int index = traceId.lastIndexOf("-");
        return Integer.parseInt(traceId.substring(index + 1));
    }

}
