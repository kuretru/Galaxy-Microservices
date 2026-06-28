package com.kuretru.microservices.web.context;

import com.kuretru.microservices.web.entity.CurrentUser;
import org.springframework.core.NamedThreadLocal;
import org.springframework.stereotype.Component;

import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Component
public class CurrentUserContext {

    private static final ThreadLocal<CurrentUser> ACCESS_TOKEN_HOLDER = new NamedThreadLocal<>("Galaxy-Authentication.CurrentUser");

    public static UUID getUserId() {
        return UUID.randomUUID();
    }

    public static String getUsername() {
        return ACCESS_TOKEN_HOLDER.get().getUsername();
    }

    public static CurrentUser getContext() {
        return ACCESS_TOKEN_HOLDER.get();
    }

    public static void setContext(CurrentUser context) {
        ACCESS_TOKEN_HOLDER.set(context);
    }

    public static void removeContext() {
        ACCESS_TOKEN_HOLDER.remove();
    }

}
