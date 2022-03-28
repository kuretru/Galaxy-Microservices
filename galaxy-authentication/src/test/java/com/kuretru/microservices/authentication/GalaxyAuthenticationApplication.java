package com.kuretru.microservices.authentication;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class})
public class GalaxyAuthenticationApplication {

    public static void main(String[] args) {
        SpringApplication.run(GalaxyAuthenticationApplication.class, args);
    }

}
