package com.kuretru.microservices.web.entity.transfer;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

import java.time.Instant;

@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public abstract class BaseCreateUpdateDTO extends BaseCreateDTO {

    @Schema(description = "记录上一次被动更新的时刻")
    private Instant updateTime;

    @Schema(description = "上一次被动更新记录的用户")
    private String updateBy;

}
