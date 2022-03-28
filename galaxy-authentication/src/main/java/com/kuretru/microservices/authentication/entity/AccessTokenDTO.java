package com.kuretru.microservices.authentication.entity;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 向前端传输的AccessToken实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class AccessTokenDTO {

    private String id;

    private String secret;

}
