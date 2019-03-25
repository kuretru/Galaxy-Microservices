package com.kuretru.api.common.controller;

import com.kuretru.api.common.util.StringUtils;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Getter
public abstract class BaseController {

    private static final String UNKNOWN_ADDRESS = "unknown";

    @Autowired
    protected HttpServletRequest request;
    @Autowired
    protected HttpSession session;

    public String getRemoteAddress() {
        // 1.先尝试取X-Forwarded-For
        String ip = request.getHeader("X-Forwarded-For");
        if (unresolvedAddress(ip)) {
            // 2.尝试取X-Real-IP
            ip = request.getHeader("X-Real-IP");
        } else {
            int index = ip.indexOf(",");
            // 取第一个地址
            if (index != -1) {
                ip = ip.substring(0, index);
            }
        }
        if (unresolvedAddress(ip)) {
            // 3.取其他的头
            ip = request.getHeader("Proxy-Client-IP");
        }
        if (unresolvedAddress(ip)) {
            // 4.取RemoteAddr
            ip = request.getRemoteAddr();
        }
        ip = "0:0:0:0:0:0:0:1".equals(ip) ? "127.0.0.1" : ip;
        return ip;
    }

    private boolean unresolvedAddress(String address) {
        return StringUtils.isNullOrEmpty(address) || UNKNOWN_ADDRESS.equals(address);
    }

}
