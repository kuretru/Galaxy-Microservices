package com.kuretru.microservices.authentication.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;
import java.util.UUID;

/**
 * 内部传输的AccessToken的实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class AccessTokenBO {

    private String secret;

    private UUID userId;

    private Set<String> roles;

}
