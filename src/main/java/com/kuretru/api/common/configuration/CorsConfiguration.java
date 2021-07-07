package com.kuretru.api.common.configuration;

import com.kuretru.api.common.constant.EnvironmentConstants;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * CORS配置
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
@Profile(EnvironmentConstants.DEVELOPMENT)
public class CorsConfiguration implements WebMvcConfigurer {

    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOriginPatterns("*")
                .maxAge(3600)
                .allowCredentials(true)
                .allowedMethods("*")
                .allowedHeaders("*");
    }

}
