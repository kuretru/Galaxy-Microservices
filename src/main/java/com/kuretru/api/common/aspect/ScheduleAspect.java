package com.kuretru.api.common.aspect;

import com.kuretru.api.common.exception.ApiException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.Order;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Aspect
@Component
@Order(10)
public class ScheduleAspect {

    private final ThreadLocal<Logger> logger = new ThreadLocal<>();
    private final ThreadLocal<Long> startTime = new ThreadLocal<>();

    @Around("@annotation(scheduled)")
    public Object around(ProceedingJoinPoint joinPoint, Scheduled scheduled) throws Throwable {
        this.startTime.set(System.currentTimeMillis());
        Logger logger = LoggerFactory.getLogger(joinPoint.getTarget().getClass());
        this.logger.set(logger);

        String className = joinPoint.getTarget().getClass().getSimpleName();
        String methodName = joinPoint.getSignature().getName();

        logger.info("--------------- Start of schedule ---------------");
        // 格式：Seconds Minutes Hours DayofMonth Month DayofWeek -> TestController.ping()
        logger.info("{} -> {}.{}()", scheduled.cron(), className, methodName);

        Object result = joinPoint.proceed();

        after();
        return result;
    }

    @AfterThrowing(value = "@annotation(scheduled)", throwing = "e")
    public void exception(Scheduled scheduled, ApiException e) {
        logger.get().error("{}: {}", e.getClass().getSimpleName(), e.getMessage());
        after();
    }

    private void after() {
        long endTime = System.currentTimeMillis();
        logger.get().info("End of schedule, spend {} milliseconds.", endTime - startTime.get());
        startTime.remove();
        logger.remove();
    }

}
