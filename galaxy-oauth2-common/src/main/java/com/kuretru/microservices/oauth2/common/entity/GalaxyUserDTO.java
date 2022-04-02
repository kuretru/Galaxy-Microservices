package com.kuretru.microservices.oauth2.common.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class GalaxyUserDTO {

    private UUID id;

    private String nickname;

    private String avatar;

}
