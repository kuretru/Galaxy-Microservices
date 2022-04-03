package com.kuretru.microservices.oauth2.server.entity;

import lombok.Data;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class OAuth2ApproveQuery {

    /** 批准Token */
    @NotEmpty
    private String token;

    /** 授权的应用 */
    @NotNull
    private UUID applicationId;

    /** 授权的用户 */
    @NotNull
    private UUID userId;

    /** 授权的范围 */
    private String scope;

}
