package com.kuretru.microservices.authentication.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Set;
import java.util.UUID;

/**
 * 数据库中存储的AccessToken实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class AccessTokenDO implements Serializable {

    private String secret;

    private UUID userId;

    private Set<String> roles;

}
