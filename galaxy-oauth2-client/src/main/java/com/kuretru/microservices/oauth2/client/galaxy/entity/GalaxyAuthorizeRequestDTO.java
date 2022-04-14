package com.kuretru.microservices.oauth2.client.galaxy.entity;

import lombok.Data;

import java.util.Set;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class GalaxyAuthorizeRequestDTO {

    private Set<String> scopes;

    private String redirectUri;

}
