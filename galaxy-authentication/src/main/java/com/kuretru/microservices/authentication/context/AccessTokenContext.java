package com.kuretru.microservices.authentication.context;

import com.kuretru.microservices.authentication.entity.AccessTokenBO;
import com.kuretru.microservices.authentication.util.RoleUtils;
import org.springframework.core.NamedThreadLocal;
import org.springframework.stereotype.Component;

import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Component
public class AccessTokenContext {

    private static final ThreadLocal<AccessTokenBO.Context> ACCESS_TOKEN_HOLDER = new NamedThreadLocal<>("AccessToken");

    public static UUID getUserId() {
        return ACCESS_TOKEN_HOLDER.get().getUserId();
    }

    public static boolean hasRole(String... roles) {
        return RoleUtils.hasRole(ACCESS_TOKEN_HOLDER.get().getRoles(), roles);

    }

    public static boolean hasRoles(String... roles) {
        return RoleUtils.hasRoles(ACCESS_TOKEN_HOLDER.get().getRoles(), roles);
    }

    public static void setAccessToken(AccessTokenBO.Context context) {
        ACCESS_TOKEN_HOLDER.set(context);
    }

    public static void removeAccessToken() {
        ACCESS_TOKEN_HOLDER.remove();
    }

}
