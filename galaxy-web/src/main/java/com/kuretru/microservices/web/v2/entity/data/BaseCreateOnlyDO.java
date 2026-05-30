package com.kuretru.microservices.web.v2.entity.data;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.time.Instant;

@Data
public abstract class BaseCreateOnlyDO {

    /** 物理主键，自增 */
    @TableId(type = IdType.AUTO)
    private Long id;

    /** 记录主动创建的时刻 */
    @TableField(fill = FieldFill.INSERT)
    private Instant createTime;

    /** 主动创建记录的用户 */
    @TableField(fill = FieldFill.INSERT)
    private String createBy;
    
}
