package com.kuretru.microservices.oauth2.client.system;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
@ComponentScan("com.kuretru.microservices.oauth2.client.system")
@MapperScan("com.kuretru.microservices.oauth2.client.system.mapper")
public class OAuth2SystemConfiguration {

}
