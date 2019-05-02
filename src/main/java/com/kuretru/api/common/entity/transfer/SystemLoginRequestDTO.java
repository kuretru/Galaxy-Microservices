package com.kuretru.api.common.entity.transfer;

import lombok.Data;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
public class SystemLoginRequestDTO {

    private String username;

    private String password;

    private String code;

}
