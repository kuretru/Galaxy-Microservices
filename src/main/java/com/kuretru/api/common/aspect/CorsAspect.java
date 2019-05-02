package com.kuretru.api.common.aspect;

import com.kuretru.api.common.configuration.EnvironmentConstants;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.context.annotation.Profile;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletResponse;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Aspect
@Component
@Profile(EnvironmentConstants.DEVELOPMENT)
@Order(20)
public class CorsAspect {

    @AfterReturning("@within(org.springframework.web.bind.annotation.RequestMapping)")
    public void around() {
        HttpServletResponse response = ((ServletRequestAttributes) RequestContextHolder.getRequestAttributes()).getResponse();
        if (response != null) {
            response.setHeader("Access-Control-Allow-Origin", "*");
            response.setHeader("Access-Control-Allow-Methods", "GET,POST,PUT,PATCH,DELETE");
            response.setHeader("Access-Control-Request-Headers", "Access-Token");
        }
    }

}
