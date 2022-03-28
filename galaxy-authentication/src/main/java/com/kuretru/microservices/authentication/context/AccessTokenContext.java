package com.kuretru.microservices.authentication.context;

import org.springframework.core.NamedThreadLocal;
import org.springframework.stereotype.Component;

import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Component
public class AccessTokenContext {

    private static final ThreadLocal<UUID> USER_ID_HOLDER = new NamedThreadLocal<>("AccessToken.UserId");

    public static UUID getUserId() {
        return USER_ID_HOLDER.get();
    }

    public static void setUserId(UUID userId) {
        USER_ID_HOLDER.set(userId);
    }

    public static void removeUserId() {
        USER_ID_HOLDER.remove();
    }

}
