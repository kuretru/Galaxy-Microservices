package com.kuretru.api.common.entity.transfer;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.UUID;

/**
 * 数据传输对象父抽象类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public abstract class BaseDTO {

    @Schema(description = "记录ID")
    private UUID id;

}
