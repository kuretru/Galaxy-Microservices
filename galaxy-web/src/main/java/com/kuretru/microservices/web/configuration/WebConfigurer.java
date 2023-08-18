package com.kuretru.microservices.web.configuration;

import org.springframework.context.annotation.Configuration;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.config.annotation.PathMatchConfigurer;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
public class WebConfigurer implements WebMvcConfigurer {

    @Override
    public void configurePathMatch(PathMatchConfigurer configurer) {
        configurer.addPathPrefix("/api", p -> p.isAnnotationPresent(RestController.class));
    }

}
