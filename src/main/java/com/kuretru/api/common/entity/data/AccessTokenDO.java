package com.kuretru.api.common.entity.data;

import lombok.Data;

import java.time.Instant;
import java.util.Set;
import java.util.UUID;

/**
 * 数据库中存储的AccessToken实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class AccessTokenDO {

    private String secret;

    private UUID userId;

    private Set<String> roles;

    private Instant expireTime;

}
