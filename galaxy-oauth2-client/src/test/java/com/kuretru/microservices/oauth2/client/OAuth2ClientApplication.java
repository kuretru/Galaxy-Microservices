package com.kuretru.microservices.oauth2.client;

import com.kuretru.microservices.oauth2.client.galaxy.GalaxyClientProperty;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class})
@ComponentScan(basePackageClasses = GalaxyClientProperty.class, excludeFilters = {
        @ComponentScan.Filter(type = FilterType.REGEX, pattern = "com.kuretru.microservices.oauth2.client.*")
})
public class OAuth2ClientApplication {

    public static void main(String[] args) {
        SpringApplication.run(OAuth2ClientApplication.class, args);
    }

}
