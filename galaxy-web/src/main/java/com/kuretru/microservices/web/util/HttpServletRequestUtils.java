package com.kuretru.microservices.web.util;

import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;

/**
 * HttpServletRequest相关的静态工具类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class HttpServletRequestUtils {

    private static final String IPV4_LOOPBACK = "127.0.0.1";
    private static final String IPV6_LOOPBACK = "0:0:0:0:0:0:0:1";

    private HttpServletRequestUtils() {

    }

    /**
     * 从HttpServletRequest获取客户端IP地址
     *
     * @param request HttpRequest
     * @return 客户端IP地址
     */
    public static String getRemoteAddress(HttpServletRequest request) {
        // 推荐：使用Nginx反代时进行配置，将真实IP放入[X-Real-IP]请求头
        String ip = request.getHeader("X-Real-IP");
        if (!StringUtils.hasText(ip)) {
            // CloudFlare将真实IP放入[CF-Connecting-IP]请求头
            ip = request.getHeader("CF-Connecting-IP");
        }
        if (!StringUtils.hasText(ip)) {
            // 取[X-Forwarded-For]反代链请求头
            ip = request.getHeader("X-Forwarded-For");
            if (StringUtils.hasText(ip)) {
                int index = ip.indexOf(",");
                // 取第一个地址
                if (index != -1) {
                    ip = ip.substring(0, index);
                }
            }
        }
        if (!StringUtils.hasText(ip)) {
            // 取[Remote-Addr]请求头
            ip = request.getRemoteAddr();
        }
        if (IPV6_LOOPBACK.equals(ip)) {
            ip = IPV4_LOOPBACK;
        }
        return ip;
    }

}
