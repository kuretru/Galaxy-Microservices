package com.kuretru.microservices.oauth2.common.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;
import java.util.UUID;

/**
 * OAuth2基本三元组{application, user, permissions}
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OAuth2Triple {

    private UUID applicationId;

    private UUID userId;

    private Set<String> scopes;

}
