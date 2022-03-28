package com.kuretru.microservices.authentication.annotaion;

import java.lang.annotation.*;

/**
 * 绑定该注解的方法或类，需要经过认证+授权后才能使用
 * 默认经过认证的用户即可访问
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface RequireAuthorization {

    /**
     * 绑定的对象需要：用户含有给定的任意角色
     *
     * @return 角色
     */
    String[] hasRole() default {};

    /**
     * 绑定的对象需要：用户必须含有给定的所有角色
     *
     * @return 角色
     */
    String[] hasRoles() default {};

}
