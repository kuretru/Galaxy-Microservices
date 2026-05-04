package com.kuretru.microservices.web.v2.entity.data;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.time.Instant;

@Data
public abstract class BaseDO {

    /** 物理主键，自增 */
    @TableId(type = IdType.AUTO)
    private Long id;

    /** 记录主动创建的时刻 */
    private Instant createTime;

    /** 主动创建记录的用户 */
    private String createBy;

    /** 记录上一次被动更新的时刻 */
    private Instant updateTime;

    /** 上一次被动更新记录的用户 */
    private String updateBy;

}
