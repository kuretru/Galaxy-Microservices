package com.kuretru.microservices.authentication.configuration;

import com.kuretru.microservices.authentication.filter.CurrentUserContextFilter;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.entity.ApiResponse;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.oauth2.server.resource.InvalidBearerTokenException;
import org.springframework.security.oauth2.server.resource.web.authentication.BearerTokenAuthenticationFilter;
import org.springframework.security.web.SecurityFilterChain;
import tools.jackson.databind.ObjectMapper;

import java.nio.charset.StandardCharsets;

@Configuration
@EnableWebSecurity
public class SpringSecurityOauth2ResourceConfiguration {

    private final ObjectMapper objectMapper;
    private final CurrentUserContextFilter currentUserContextFilter;

    @Autowired
    public SpringSecurityOauth2ResourceConfiguration(ObjectMapper objectMapper, CurrentUserContextFilter currentUserContextFilter) {
        this.objectMapper = objectMapper;
        this.currentUserContextFilter = currentUserContextFilter;
    }

    @Bean
    SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
        return http
                .csrf(AbstractHttpConfigurer::disable)
                .authorizeHttpRequests(auth -> auth
                        .requestMatchers(
                                "/healthz",
                                "/login",
                                "/login/callback",
                                "/api/ping",
                                "/api/exception",
                                "/swagger-ui.html",
                                "/swagger-ui/*",
                                "/v3/api-docs",
                                "/v3/api-docs/*"
                        ).permitAll()
                        .anyRequest().authenticated()
                )
                .oauth2ResourceServer(oauth2 -> oauth2
                        .jwt(Customizer.withDefaults())
                        .authenticationEntryPoint(((request, response, authException) -> {
                            response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
                            response.setCharacterEncoding(StandardCharsets.UTF_8);
                            response.setContentType(MediaType.APPLICATION_JSON_VALUE);

                            ApiResponse<String> apiResponse;
                            if (authException instanceof InvalidBearerTokenException) {
                                apiResponse = ApiResponse.build(UserErrorCodes.ACCESS_UNAUTHORIZED, authException.getMessage());
                            } else {
                                apiResponse = ApiResponse.build(UserErrorCodes.ACCESS_UNAUTHORIZED, authException.getMessage());
                            }

                            objectMapper.writeValue(response.getWriter(), apiResponse);
                        }))
                )
                .addFilterAfter(currentUserContextFilter, BearerTokenAuthenticationFilter.class)
                .build();
    }

}
