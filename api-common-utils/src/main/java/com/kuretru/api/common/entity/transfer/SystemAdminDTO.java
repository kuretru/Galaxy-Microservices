package com.kuretru.api.common.entity.transfer;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.kuretru.api.common.entity.enums.UserRoleEnum;
import com.kuretru.api.common.util.InstantUtils;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.Instant;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SystemAdminDTO extends BaseDTO {

    private String username;

    private String nickName;

    private UserRoleEnum userRole;

    @JsonFormat(pattern = InstantUtils.GENERAL_DATE_FORMAT, timezone = InstantUtils.GENERAL_TIME_ZONE)
    private Instant lastLogin;

}
