package com.kuretru.microservices.authentication.aspect;

import com.kuretru.microservices.authentication.annotaion.RequireAuthorization;
import com.kuretru.microservices.authentication.constant.AccessTokenConstants;
import com.kuretru.microservices.authentication.context.AccessTokenContext;
import com.kuretru.microservices.authentication.entity.AccessTokenBO;
import com.kuretru.microservices.authentication.entity.AccessTokenDTO;
import com.kuretru.microservices.authentication.manager.AccessTokenManager;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.exception.ServiceException;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.core.annotation.Order;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.Set;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Aspect
@Order(20)
public class SimpleAuthorizationAspect {

    private final AccessTokenManager accessTokenManager;

    public SimpleAuthorizationAspect(AccessTokenManager accessTokenManager) {
        this.accessTokenManager = accessTokenManager;
    }

    @Before("@annotation(requireAuthorization)")
//    @Before("@annotation(requireAuthorization) || " +
//            "(@within(requireAuthorization) && execution(public com.kuretru.api.common.entity.ApiResponse *(..)))")
    public void before(JoinPoint joinPoint, RequireAuthorization requireAuthorization) throws ServiceException {
        AccessTokenDTO accessTokenDTO = getAccessTokenFromUser();
        AccessTokenBO accessTokenBO = getAccessTokenFromDatabase(accessTokenDTO.getId());
        authentication(accessTokenDTO, accessTokenBO);
        authorization(requireAuthorization, accessTokenBO);
        AccessTokenContext.setUserId(accessTokenBO.getUserId());
    }

    protected void authentication(AccessTokenDTO dto, AccessTokenBO bo) throws ServiceException {
        if (!bo.getSecret().equals(dto.getSecret())) {
            throw ServiceException.build(UserErrorCodes.USER_LOGIN_ERROR, "AccessToken不匹配");
        }
    }

    protected void authorization(RequireAuthorization requireAuthorization, AccessTokenBO accessTokenBO) throws ServiceException {
        if (requireAuthorization.hasRole().length > 0 && requireAuthorization.hasRoles().length > 0) {
            throw new IllegalArgumentException("RequireAuthorization不能同时配置hasRole和hasRoles属性");
        }
        if (requireAuthorization.hasRole().length == 0 && requireAuthorization.hasRoles().length == 0) {
            return;
        }

        Set<String> roles = accessTokenBO.getRoles();
        if (requireAuthorization.hasRole().length > 0) {
            for (String role : requireAuthorization.hasRole()) {
                if (roles.contains(role)) {
                    return;
                }
            }
            throw ServiceException.build(UserErrorCodes.ACCESS_PERMISSION_ERROR, "用户缺少权限");
        } else {
            for (String role : requireAuthorization.hasRoles()) {
                if (!roles.contains(role)) {
                    throw ServiceException.build(UserErrorCodes.ACCESS_PERMISSION_ERROR, "用户缺少权限");
                }
            }
        }
    }

    private AccessTokenDTO getAccessTokenFromUser() throws ServiceException {
        ServletRequestAttributes servletRequestAttributes = (ServletRequestAttributes)RequestContextHolder.getRequestAttributes();
        if (servletRequestAttributes == null) {
            throw ServiceException.build(UserErrorCodes.USER_LOGIN_ERROR, "无法获得AccessToken");
        }
        HttpServletRequest request = servletRequestAttributes.getRequest();
        AccessTokenDTO accessTokenDTO = (AccessTokenDTO)request.getAttribute(AccessTokenConstants.ACCESS_TOKEN_ATTRIBUTE);
        if (accessTokenDTO == null) {
            throw ServiceException.build(UserErrorCodes.USER_LOGIN_ERROR, "请求头未携带AccessToken");
        }
        request.removeAttribute(AccessTokenConstants.ACCESS_TOKEN_ATTRIBUTE);
        return accessTokenDTO;
    }

    private AccessTokenBO getAccessTokenFromDatabase(String accessTokenId) throws ServiceException {
        return accessTokenManager.get(accessTokenId);
    }

}
