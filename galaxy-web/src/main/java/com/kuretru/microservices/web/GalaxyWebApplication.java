package com.kuretru.microservices.web;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@SpringBootApplication(exclude = {DataSourceAutoConfiguration.class})
public class GalaxyWebApplication {

    public static void main(String[] args) {
        System.setProperty("server.servlet.context-path", "/api");
        SpringApplication.run(GalaxyWebApplication.class, args);
    }

}
