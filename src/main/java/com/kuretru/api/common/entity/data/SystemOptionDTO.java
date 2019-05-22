package com.kuretru.api.common.entity.data;

import com.kuretru.api.common.entity.transfer.BaseDTO;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SystemOptionDTO extends BaseDTO {

    private String key;

    private String value;

}
