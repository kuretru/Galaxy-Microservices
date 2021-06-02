package com.kuretru.api.common.entity.data;

import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@EqualsAndHashCode(callSuper = true)
public abstract class BaseSequenceDO extends BaseDO {

    /** 排序依据 */
    private Integer sequence;

}
