package com.kuretru.api.common.entity.business;

import lombok.Data;

import java.util.UUID;

/**
 * 内部验证AccessToken的实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class AccessTokenBO {

    private String secret;

    private UUID userId;

}
