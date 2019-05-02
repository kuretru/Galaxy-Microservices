package com.kuretru.api.common.aspect;

import com.kuretru.api.common.controller.BaseController;
import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.exception.ApiException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Aspect
@Component
@Order(10)
public class LoggerAspect {

    private ThreadLocal<Logger> logger = new ThreadLocal<>();
    private ThreadLocal<Long> startTime = new ThreadLocal<>();

    @Around("pointCut()")
    public ApiResponse around(ProceedingJoinPoint joinPoint) throws Throwable {
        this.startTime.set(System.currentTimeMillis());
        BaseController baseController = (BaseController) joinPoint.getTarget();
        HttpServletRequest request = baseController.getRequest();
        Logger logger = LoggerFactory.getLogger(baseController.getClass());
        this.logger.set(logger);

        String remoteAddress = baseController.getRemoteAddress();
        String httpMethod = request.getMethod();
        String path = request.getServletPath();
        String className = baseController.getClass().getSimpleName();
        String methodName = joinPoint.getSignature().getName();

        logger.info("--------------- Start of request ---------------");
        // 格式：127.0.0.1 GET /ping @TestController.ping()
        logger.info("{} {} {} @{}.{}()", remoteAddress, httpMethod, path, className, methodName);

        ApiResponse apiResponse = (ApiResponse) joinPoint.proceed();

        after();
        return apiResponse;
    }

    @AfterThrowing(value = "pointCut()", throwing = "e")
    public void exception(ApiException e) {
        logger.get().error("{}: {}", e.getClass().getSimpleName(), e.getMessage());
        after();
    }

    private void after() {
        long endTime = System.currentTimeMillis();
        logger.get().info("End of request, spend {} milliseconds.", endTime - startTime.get());
        startTime.remove();
        logger.remove();
    }

    @Pointcut("execution(com.kuretru.api.common.entity.ApiResponse *(..)) && @target(org.springframework.web.bind.annotation.RestController)")
    private void pointCut() {
    }

}
