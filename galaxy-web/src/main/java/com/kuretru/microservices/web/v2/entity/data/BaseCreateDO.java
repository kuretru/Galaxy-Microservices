package com.kuretru.microservices.web.v2.entity.data;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

import java.time.Instant;

@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public abstract class BaseCreateDO extends BaseDO {

    /** 记录主动创建的时刻 */
    @TableField(fill = FieldFill.INSERT)
    private Instant createTime;

    /** 主动创建记录的用户 */
    @TableField(fill = FieldFill.INSERT)
    private String createBy;

}
