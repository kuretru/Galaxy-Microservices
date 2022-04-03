package com.kuretru.microservices.oauth2.server.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Set;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OAuth2ApprovedDO implements Serializable {

    private String redirectUri;

    private String clientId;

    private UUID userId;

    private Set<String> scopes;

}
