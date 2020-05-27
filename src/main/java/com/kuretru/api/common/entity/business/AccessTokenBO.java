package com.kuretru.api.common.entity.business;

import com.kuretru.api.common.entity.enums.UserRoleEnum;
import com.kuretru.api.common.util.HashUtils;
import com.kuretru.api.common.util.StringUtils;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.NonNull;

import java.util.UUID;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AccessTokenBO {

    private static final Integer ARGS_COUNT = 2;

    private Long id;

    private UserRoleEnum role;

    private String token;

    public static AccessTokenBO build(@NonNull Long id, @NonNull UserRoleEnum role) {
        String uuid = UUID.randomUUID().toString();
        String token = HashUtils.computeMd5(uuid + role.toString());
        return new AccessTokenBO(id, role, token);
    }

    public static AccessTokenBO fromRedis(String idAndRole, String token) {
        if (StringUtils.isNullOrEmpty(idAndRole)) {
            return null;
        }
        String[] args = idAndRole.split("@");
        if (ARGS_COUNT != args.length) {
            return null;
        }
        Long id = Long.valueOf(args[0]);
        UserRoleEnum role = UserRoleEnum.valueOf(args[1]);
        return new AccessTokenBO(id, role, token);
    }

    public String getIdAndRole() {
        return this.id + "@" + this.role.name();
    }

}
