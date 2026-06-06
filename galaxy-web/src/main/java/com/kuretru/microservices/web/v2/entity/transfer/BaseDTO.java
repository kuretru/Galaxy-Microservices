package com.kuretru.microservices.web.v2.entity.transfer;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

@Data
public abstract class BaseDTO {

    @Schema(description = "记录ID")
    private Long id;

}
