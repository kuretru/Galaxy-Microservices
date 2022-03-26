package com.kuretru.microservices.web.configuration;

import com.kuretru.microservices.web.aspect.LoggingAspect;
import com.kuretru.microservices.web.controller.ExceptionController;
import com.kuretru.microservices.web.controller.TestController;
import com.kuretru.microservices.web.manager.impl.TraceIdManagerImpl;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
@Import({CorsConfiguration.class, MybatisPlusConfiguration.class})
public class GalaxyWebAutoConfiguration {

    @Configuration
    @Import(LoggingAspect.class)
    protected static class AspectConfiguration {

    }

    @Configuration
    @Import({ExceptionController.class, TestController.class})
    protected static class ControllerConfiguration {

    }

    @Configuration
    @Import(TraceIdManagerImpl.class)
    protected static class ManagerConfiguration {

    }

}
