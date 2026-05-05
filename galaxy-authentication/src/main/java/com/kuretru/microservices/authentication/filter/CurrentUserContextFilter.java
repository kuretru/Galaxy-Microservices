package com.kuretru.microservices.authentication.filter;

import com.kuretru.microservices.web.context.CurrentUserContext;
import com.kuretru.microservices.web.entity.CurrentUser;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.oauth2.jwt.Jwt;
import org.springframework.security.oauth2.server.resource.authentication.JwtAuthenticationToken;
import org.springframework.stereotype.Component;
import org.springframework.web.filter.OncePerRequestFilter;

import java.io.IOException;

@Component
public class CurrentUserContextFilter extends OncePerRequestFilter {

    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain filterChain) throws ServletException, IOException {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication instanceof JwtAuthenticationToken jwtAuthenticationToken && authentication.isAuthenticated()) {
            CurrentUserContext.removeContext();

            Jwt jwt = jwtAuthenticationToken.getToken();

            CurrentUser currentUser = new CurrentUser();
            currentUser.setUsername(jwt.getClaimAsString("sub"));
            currentUser.setNickname(jwt.getClaimAsString("name"));
            currentUser.setEmail(jwt.getClaimAsString("email"));

            CurrentUserContext.setContext(currentUser);
        }

        filterChain.doFilter(request, response);
    }

}
