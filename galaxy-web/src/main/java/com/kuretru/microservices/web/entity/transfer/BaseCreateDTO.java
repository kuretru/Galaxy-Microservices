package com.kuretru.microservices.web.entity.transfer;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

import java.time.Instant;

@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public abstract class BaseCreateDTO extends BaseDTO {

    @Schema(description = "记录主动创建的时刻")
    private Instant createTime;

    @Schema(description = "主动创建记录的用户")
    private String createBy;

}
