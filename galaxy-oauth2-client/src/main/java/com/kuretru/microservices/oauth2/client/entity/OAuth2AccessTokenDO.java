package com.kuretru.microservices.oauth2.client.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Set;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OAuth2AccessTokenDO implements Serializable {

    private String accessToken;

    private String refreshToken;

    private Set<String> scopes;

}
