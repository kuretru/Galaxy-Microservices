package com.kuretru.microservices.web.v2.entity.transfer;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.time.Instant;

@Data
public abstract class BaseCreateOnlyDTO {

    @Schema(description = "记录ID")
    private Long id;

    @Schema(description = "记录主动创建的时刻")
    private Instant createTime;

    @Schema(description = "主动创建记录的用户")
    private String createBy;

}
