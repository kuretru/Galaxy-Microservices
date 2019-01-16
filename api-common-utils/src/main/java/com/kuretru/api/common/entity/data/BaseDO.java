package com.kuretru.api.common.entity.data;

import com.baomidou.mybatisplus.annotation.TableId;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.kuretru.api.common.util.InstantUtils;
import lombok.Data;

import java.time.Instant;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
public abstract class BaseDO {

    @TableId
    private Long id;

    @JsonFormat(pattern = InstantUtils.GENERAL_DATE_FORMAT, timezone = InstantUtils.GENERAL_TIME_ZONE)
    private Instant createAt;

    private Instant modifiedAt;

}
