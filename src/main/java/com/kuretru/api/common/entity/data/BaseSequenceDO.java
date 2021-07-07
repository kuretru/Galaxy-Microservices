package com.kuretru.api.common.entity.data;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public abstract class BaseSequenceDO extends BaseDO {

    /** 排序依据 */
    private Integer sequence;

}
