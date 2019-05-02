package com.kuretru.api.common.annotation;

import java.lang.annotation.*;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Target(ElementType.PARAMETER)
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface BindUser {

    /**
     * 在POJO中要绑定相关用户ID的属性名
     *
     * @return 属性名
     */
    String value() default "";

}
