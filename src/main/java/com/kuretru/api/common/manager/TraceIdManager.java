package com.kuretru.api.common.manager;

/**
 * TraceID管理器
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface TraceIdManager {

    /**
     * 生成一个TraceId
     *
     * @return TraceID
     */
    String generateTraceId();

}
