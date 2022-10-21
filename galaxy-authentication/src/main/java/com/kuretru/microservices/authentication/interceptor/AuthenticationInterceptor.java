package com.kuretru.microservices.authentication.interceptor;

import com.kuretru.microservices.authentication.constant.AccessTokenConstants;
import com.kuretru.microservices.authentication.context.AccessTokenContext;
import com.kuretru.microservices.authentication.entity.AccessTokenDTO;
import org.jetbrains.annotations.NotNull;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.HandlerInterceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Component
public class AuthenticationInterceptor implements HandlerInterceptor {

    @Override
    public boolean preHandle(HttpServletRequest request, @NotNull HttpServletResponse response, @NotNull Object handler) throws Exception {
        AccessTokenContext.removeAccessToken();
        String id = request.getHeader(AccessTokenConstants.ACCESS_TOKEN_ID);
        if (StringUtils.hasText(id)) {
            String secret = request.getHeader(AccessTokenConstants.ACCESS_TOKEN);
            AccessTokenDTO accessTokenDTO = new AccessTokenDTO(id, secret);
            request.setAttribute(AccessTokenConstants.ACCESS_TOKEN_ATTRIBUTE, accessTokenDTO);
        }
        return HandlerInterceptor.super.preHandle(request, response, handler);
    }

}
