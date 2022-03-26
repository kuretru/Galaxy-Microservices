package com.kuretru.microservices.web.aspect;

import com.kuretru.microservices.web.constant.LoggingConstants;
import com.kuretru.microservices.web.constant.code.ResponseCodes;
import com.kuretru.microservices.web.controller.BaseController;
import com.kuretru.microservices.web.entity.ApiResponse;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.manager.TraceIdManager;
import com.kuretru.microservices.web.util.HttpServletRequestUtils;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import java.lang.reflect.UndeclaredThrowableException;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Aspect
@Component
@Order(10)
@Slf4j
public class LoggingAspect {

    private final TraceIdManager traceIdManager;

    @Autowired
    public LoggingAspect(TraceIdManager traceIdManager) {
        this.traceIdManager = traceIdManager;
    }

    @Around("pointcut()")
    public ApiResponse<?> around(ProceedingJoinPoint joinPoint) throws Throwable {
        long startTime = System.currentTimeMillis();
        BaseController controller = (BaseController)joinPoint.getTarget();
        HttpServletRequest request = controller.getRequest();
        MDC.clear();
        MDC.put(LoggingConstants.TRACE_ID_KEY, traceIdManager.generateTraceId());

        String remoteAddress = HttpServletRequestUtils.getRemoteAddress(request);
        String httpMethod = request.getMethod();
        String path = request.getServletPath();
        String methodName = joinPoint.getSignature().getName();
        String className = controller.getClass().getSimpleName();

        if (log.isInfoEnabled()) {
            log.info("--------------- Start of request ---------------");
            log.info("{} {} {} @{}.{}()", remoteAddress, httpMethod, path, className, methodName);
        }

        try {
            return (ApiResponse<?>)joinPoint.proceed();
        } finally {
            if (log.isInfoEnabled()) {
                long endTime = System.currentTimeMillis();
                log.info("End of request, spend {} milliseconds.", endTime - startTime);
            }
        }
    }

    @AfterThrowing(value = "pointcut()", throwing = "t")
    public void afterThrowing(Throwable t) {
        if (t instanceof UndeclaredThrowableException) {
            t = t.getCause();
        }
        if (t instanceof ServiceException) {
            ResponseCodes code = ((ServiceException)t).getCode();
            log.error("{}({},{}): {}", t.getClass().getSimpleName(), code.getCode(), code.getMessage(), t.getMessage());
        } else {
            log.error("{}: {}", t.getClass().getSimpleName(), t.getMessage());
        }
    }

    @Pointcut("execution(com.kuretru.microservices.web.entity.ApiResponse *(..)) && " +
            "@target(org.springframework.web.bind.annotation.RestController)")
    private void pointcut() {

    }

}
