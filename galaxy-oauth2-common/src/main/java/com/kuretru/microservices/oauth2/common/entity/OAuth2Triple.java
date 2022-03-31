package com.kuretru.microservices.oauth2.common.entity;

import lombok.Data;

import java.util.Set;
import java.util.UUID;

/**
 * OAuth2基本三元组{application, user, permissions}
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public abstract class OAuth2Triple {

    private UUID applicationId;

    private UUID userId;

    private Set<String> scopes;

}
