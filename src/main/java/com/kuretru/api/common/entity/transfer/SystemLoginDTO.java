package com.kuretru.api.common.entity.transfer;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SystemLoginDTO extends SystemAdminDTO {

    private String accessToken;

}
