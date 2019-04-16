package com.kuretru.api.common.entity.data;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.Instant;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("sys_admin")
public class SystemAdminDO extends BaseDO {

    private String username;

    private String password;

    private String salt;

    private String nickName;

    private Integer userRole;

    private Instant lastLogin;

}
