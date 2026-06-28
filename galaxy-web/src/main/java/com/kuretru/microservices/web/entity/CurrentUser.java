package com.kuretru.microservices.web.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 内部传输的AccessToken的实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class CurrentUser {

    private String userJwtToken;

    private String username;

    private String nickname;

    private String email;

}
