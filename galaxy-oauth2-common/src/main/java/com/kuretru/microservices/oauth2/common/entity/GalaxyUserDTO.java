package com.kuretru.microservices.oauth2.common.entity;

import lombok.Data;

import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class GalaxyUserDTO {

    private UUID id;

    private String nickname;

    private String avatar;

}
