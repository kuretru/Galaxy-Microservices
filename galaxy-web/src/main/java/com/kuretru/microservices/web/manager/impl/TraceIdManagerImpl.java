package com.kuretru.microservices.web.manager.impl;

import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.web.manager.TraceIdManager;
import lombok.SneakyThrows;
import org.springframework.stereotype.Service;

import java.net.InetAddress;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
public class TraceIdManagerImpl implements TraceIdManager {

    public static final int MIN_SEQUENCE = 1000;
    public static final int MAX_SEQUENCE = 9000;

    private final String LOCAL_IP;
    private final String PID;
    private int sequence = MIN_SEQUENCE;

    public TraceIdManagerImpl() {
        this.LOCAL_IP = getLocalIp();
        long pid = ProcessHandle.current().pid();
        this.PID = String.format("%05d", pid % 10000);
    }

    @Override
    public String generateTraceId() {
        long now = System.currentTimeMillis();
        return String.format("%s-%013d-%s-%04d", LOCAL_IP, now, PID, nextSequence());
    }

    private synchronized int nextSequence() {
        int result = this.sequence++;
        if (sequence >= MAX_SEQUENCE) {
            sequence = MIN_SEQUENCE;
        }
        return result;
    }

    @SneakyThrows
    private String getLocalIp() {
        InetAddress ip = InetAddress.getLocalHost();
        byte[] bytes = ip.getAddress();
        return StringUtils.bytesToHexString(bytes);
    }

}
