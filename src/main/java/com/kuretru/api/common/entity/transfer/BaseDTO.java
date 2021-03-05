package com.kuretru.api.common.entity.transfer;

import lombok.Data;

import java.util.UUID;

/**
 * 数据传输对象父抽象类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public abstract class BaseDTO {

    /** 业务逻辑主键 */
    private UUID id;

}
