package com.kuretru.microservices.oauth2.server.entity;

import lombok.Data;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class OAuth2AccessTokenBO {

    private String accessToken;

    private Long expiresIn;

    private String refreshToken;

}
