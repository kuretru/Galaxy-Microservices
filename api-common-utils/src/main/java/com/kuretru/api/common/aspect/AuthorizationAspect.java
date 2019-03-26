package com.kuretru.api.common.aspect;

import com.kuretru.api.common.annotation.RequestAuthorization;
import com.kuretru.api.common.configuration.GeneralConstants;
import com.kuretru.api.common.entity.business.AccessTokenBO;
import com.kuretru.api.common.exception.ApiException;
import com.kuretru.api.common.exception.AuthenticationFailedException;
import com.kuretru.api.common.manager.AccessTokenManager;
import com.kuretru.api.common.util.StringUtils;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import javax.servlet.http.HttpServletRequest;

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

    @Before("@annotation(requestAuthorization)")
    public void before(RequestAuthorization requestAuthorization) throws ApiException {
        String header = request.getHeader(GeneralConstants.ACCESS_TOKEN_HEADER);

        // 1.判断是否有请求头
        if (StringUtils.isNullOrEmpty(header)) {
            throw new AuthenticationFailedException("缺少请求头" + GeneralConstants.ACCESS_TOKEN_HEADER);
        }
        // 2.万能Token
        if (GeneralConstants.MAGIC_TOKEN.equals(header)) {
            return;
        }
        // 3.验证角色是否有该权限
        AccessTokenBO model = accessTokenManager.verifyToken(header);
        if (requestAuthorization.requiredRole().getCode() > model.getRole().getCode()) {
            throw new AuthenticationFailedException("该用户没有操作此接口的权限");
        }
    }

}
