package com.kuretru.microservices.oauth2.server.entity;

import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.oauth2.common.entity.OAuth2Triple;
import lombok.*;

import java.io.Serializable;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
@NoArgsConstructor
@AllArgsConstructor
public class OAuth2AccessTokenDO extends OAuth2Triple implements Serializable {

    private String refreshToken;

    public OAuth2AccessTokenDO(OAuth2Triple triple) {
        this.setApplicationId(triple.getApplicationId());
        this.setUserId(triple.getUserId());
        this.setScopes(triple.getScopes());
        this.refreshToken = StringUtils.randomUUID();
    }

}
