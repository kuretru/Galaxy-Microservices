package com.kuretru.microservices.oauth2.client.system.entity;

import lombok.Data;

import javax.validation.constraints.Size;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class UserQuery {

    @Size(max = 16)
    private String nickname;

}
