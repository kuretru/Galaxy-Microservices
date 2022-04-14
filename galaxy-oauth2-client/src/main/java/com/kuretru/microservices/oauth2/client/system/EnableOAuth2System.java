package com.kuretru.microservices.oauth2.client.system;

import org.springframework.context.annotation.Import;

import java.lang.annotation.*;

/**
 * 启用基于Galaxy OAuth2 Server的基本业务逻辑
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Import(OAuth2SystemConfiguration.class)
public @interface EnableOAuth2System {

}
