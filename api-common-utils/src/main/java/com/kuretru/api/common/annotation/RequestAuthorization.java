package com.kuretru.api.common.annotation;

import com.kuretru.api.common.entity.enums.UserRoleEnum;

import java.lang.annotation.*;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface RequestAuthorization {

    /**
     * 要求的角色等级
     *
     * @return 角色等级
     */
    UserRoleEnum requiredRole() default UserRoleEnum.USER;

}
