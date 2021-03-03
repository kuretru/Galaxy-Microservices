package com.kuretru.common.entity;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.time.Instant;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public abstract class BaseDO {

    @TableId(type = IdType.AUTO)
    private Long id;

    private String uuid;

    private Instant createTime;

    private Instant updateTime;

}
