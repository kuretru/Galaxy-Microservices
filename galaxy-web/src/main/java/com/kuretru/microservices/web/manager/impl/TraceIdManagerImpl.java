package com.kuretru.microservices.web.manager.impl;

import com.kuretru.microservices.common.utils.InstantUtils;
import com.kuretru.microservices.common.utils.NetworkUtils;
import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.web.manager.TraceIdManager;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.net.InetAddress;
import java.time.Instant;
import java.time.format.DateTimeFormatter;

import static com.kuretru.microservices.common.utils.InstantUtils.DEFAULT_ZONE_ID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Slf4j
@Service
public class TraceIdManagerImpl implements TraceIdManager {

    public static final int MIN_SEQUENCE = 1000;
    public static final int MAX_SEQUENCE = 99000;
    private static final String DATETIME_FORMAT = "yyyyMMdd-HHmmss";
    private static final DateTimeFormatter DATETIME_FORMATTER =
            DateTimeFormatter.ofPattern(DATETIME_FORMAT).withZone(DEFAULT_ZONE_ID);

    private final String LOCAL_IP;
    private final String PID;
    private int sequence = MIN_SEQUENCE;

    public TraceIdManagerImpl() {
        this.LOCAL_IP = getLocalIp();
        long pid = ProcessHandle.current().pid();
        this.PID = String.format("%05d", pid % 100000);
    }

    @Override
    public String generateTraceId() {
        String now = InstantUtils.toString(Instant.now(), DATETIME_FORMATTER);
        // 8        8        6      5      5        共计32+4位
        // IPv4     Date     Time   PID    Sequence
        // 7F000001_20230815-222715_12345_12345
        return String.format("%s_%s_%s_%05d", LOCAL_IP, now, PID, nextSequence());
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
        InetAddress ip = NetworkUtils.getLocalIpv4Address();
        byte[] bytes = ip.getAddress();
        String result = StringUtils.bytesToHexString(bytes);
        return result.substring(0, 8);
    }

}
