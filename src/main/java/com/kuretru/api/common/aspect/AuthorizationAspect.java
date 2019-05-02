package com.kuretru.api.common.aspect;

import com.kuretru.api.common.annotation.BindUser;
import com.kuretru.api.common.annotation.RequestAuthorization;
import com.kuretru.api.common.configuration.EnvironmentConstants;
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
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Aspect
@Component
@Order(20)
public class AuthorizationAspect {

    private Environment environment;

    private HttpServletRequest request;

    private AccessTokenManager accessTokenManager;

    @Autowired
    public AuthorizationAspect(Environment environment, HttpServletRequest request, AccessTokenManager accessTokenManager) {
        this.environment = environment;
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
            // 2.测试环境万能Token
            String activeProfile = environment.getActiveProfiles()[0];
            if (EnvironmentConstants.DEVELOPMENT.equalsIgnoreCase(activeProfile)) {
                tokenBO = AccessTokenBO.build(1L, UserRoleEnum.ADMIN);
            } else {
                throw new AuthenticationFailedException("当前环境无调试权限");
            }
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

    private Object[] bindUser(AccessTokenBO tokenBO, ProceedingJoinPoint joinPoint) throws Throwable {
        // 获取所有入参
        Object[] args = joinPoint.getArgs();
        // 获取调用的方法
        MethodSignature methodSignature = (MethodSignature) joinPoint.getSignature();
        Method method = methodSignature.getMethod();
        Parameter[] parameters = method.getParameters();
        bind:
        for (int i = 0; i < parameters.length; i++) {
            Annotation[] annotations = parameters[i].getAnnotations();
            for (Annotation annotation : annotations) {
                if (!(annotation instanceof BindUser)) {
                    continue;
                }
                Class c = parameters[i].getType();
                // 如果绑定对象是Long的情况
                if (c.equals(Long.class)) {
                    if (args[i] == null) {
                        // 说明这是一个需要直接绑定的参数
                        args[i] = tokenBO.getId();
                    } else {
                        // 说明这是一个存在于URL中的参数，进一步检查两者是否相等
                        if (!tokenBO.getId().equals(args[i])) {
                            throw new AuthenticationFailedException("请勿操作别人的数据");
                        }
                    }
                    break bind;
                }
                // 如果绑定对象是其他POJO类型的情况，进一步设置其属性
                String property = ((BindUser) annotation).value();
                if (StringUtils.isNullOrEmpty(property)) {
                    throw new NullPointerException("未指定要绑定的POJO中的属性名称");
                }
                if (args[i] == null) {
                    args[i] = c.newInstance();
                }
                Field field = args[i].getClass().getDeclaredField(property);
                field.setAccessible(true);
                field.set(args[i], tokenBO.getId());
            }
        }
        return args;
    }

}
