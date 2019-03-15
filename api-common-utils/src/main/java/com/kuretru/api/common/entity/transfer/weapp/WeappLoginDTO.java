package com.kuretru.api.common.entity.transfer.weapp;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class WeappLoginDTO extends WeappUserDTO {

    private String accessToken;

}
