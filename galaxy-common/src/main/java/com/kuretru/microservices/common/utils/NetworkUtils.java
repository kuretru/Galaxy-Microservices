package com.kuretru.microservices.common.utils;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Slf4j
public class NetworkUtils {

    private NetworkUtils() {

    }

    /**
     * 尽量获取本机IPv4地址
     *
     * @return 本机IPv4地址
     */
    @SneakyThrows
    public static InetAddress getLocalIpv4Address() {
        List<InetAddress> siteLocalAddresses = new ArrayList<>();
        List<InetAddress> linkLocalAddresses = new ArrayList<>();
        List<InetAddress> loopbackAddress = new ArrayList<>();

        try {
            NetworkInterface.networkInterfaces().flatMap(NetworkInterface::inetAddresses).forEach(
                    inetAddress -> {
                        if (inetAddress instanceof Inet6Address) {
                            return;
                        }
                        if (inetAddress.isSiteLocalAddress()) {
                            siteLocalAddresses.add(inetAddress);
                        } else if (inetAddress.isLinkLocalAddress()) {
                            linkLocalAddresses.add(inetAddress);
                        } else if (inetAddress.isLoopbackAddress()) {
                            loopbackAddress.add(inetAddress);
                        }
                    }
            );
        } catch (SocketException e) {
            log.error("查找本机IP失败，" + e.getMessage());
        }

        // 1. 尝试找Site Local地址
        if (!siteLocalAddresses.isEmpty()) {
            return siteLocalAddresses.get(0);
        }
        // 2. 尝试找Link Local地址，169.254.x.x
        if (!linkLocalAddresses.isEmpty()) {
            return linkLocalAddresses.get(0);
        }
        // 3. 尝试找Loopback地址，127.x.x.x
        if (!loopbackAddress.isEmpty()) {
            return loopbackAddress.get(0);
        }
        // 4. 用127.0.0.1兜底
        return InetAddress.getByName("127.0.0.1");
    }

}
