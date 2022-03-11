package com.kuretru.api.common.entity.transfer;

import lombok.Data;

/**
 * 向前端传输的AccessToken实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class AccessTokenDTO {

    private String id;

    private String secret;

}
