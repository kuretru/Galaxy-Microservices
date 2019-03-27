package com.kuretru.api.common.aspect;

import com.kuretru.api.common.annotation.BindUser;
import com.kuretru.api.common.annotation.RequestAuthorization;
import com.kuretru.api.common.configuration.GeneralConstants;
import com.kuretru.api.common.entity.business.AccessTokenBO;
import com.kuretru.api.common.entity.enums.UserRoleEnum;
import com.kuretru.api.common.exception.AuthenticationFailedException;
import com.kuretru.api.common.manager.AccessTokenManager;
import com.kuretru.api.common.util.StringUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Aspect
@Component
@Order(20)
public class AuthorizationAspect {

    private HttpServletRequest request;

    private AccessTokenManager accessTokenManager;

    @Autowired
    public AuthorizationAspect(HttpServletRequest request, AccessTokenManager accessTokenManager) {
        this.request = request;
        this.accessTokenManager = accessTokenManager;
    }

    @Around("@annotation(requestAuthorization)")
    public Object around(ProceedingJoinPoint joinPoint, RequestAuthorization requestAuthorization) throws Throwable {
        String header = request.getHeader(GeneralConstants.ACCESS_TOKEN_HEADER);

        // 1.判断是否有请求头
        if (StringUtils.isNullOrEmpty(header)) {
            throw new AuthenticationFailedException("缺少请求头" + GeneralConstants.ACCESS_TOKEN_HEADER);
        }

        AccessTokenBO tokenBO;
        if (GeneralConstants.MAGIC_TOKEN.equals(header)) {
            // 2.万能Token
            tokenBO = AccessTokenBO.build(1L, UserRoleEnum.ADMIN);
        } else {
            // 2.正常查询Redis
            tokenBO = accessTokenManager.verifyToken(header);
        }

        // 3.验证角色是否有该权限
        if (requestAuthorization.requiredRole().getCode() > tokenBO.getRole().getCode()) {
            throw new AuthenticationFailedException("该用户没有操作此接口的权限");
        }

        // 4.获取添加用户ID后的入参
        Object[] args = bindUser(tokenBO, joinPoint);
        return joinPoint.proceed(args);
    }

    private Object[] bindUser(AccessTokenBO tokenBO, ProceedingJoinPoint joinPoint) {
        Object[] args = joinPoint.getArgs();

        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
        Method method = methodSignature.getMethod();
        Parameter[] parameters = method.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            Annotation[] annotations = parameters[i].getAnnotations();
            for (Annotation annotation : annotations) {
                if (!(annotation instanceof BindUser)) {
                    continue;
                }
                // 如果绑定对象是Long的情况
                if (parameters[i].getType().equals(Long.class)) {
                    args[i] = tokenBO.getId();
                }
                // TODO 如果绑定对象是其他POJO的情况
            }
        }
        return args;
    }

}
