package com.kuretru.api.common.entity.transfer;

import com.kuretru.api.common.entity.Indexable;
import lombok.Data;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
public abstract class BaseDTO implements Indexable {

    private Long id;

}
