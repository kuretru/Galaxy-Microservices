package com.kuretru.microservices.web.entity.data;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;

import java.time.Instant;

@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public abstract class BaseCreateUpdateDO extends BaseCreateDO {

    /** 记录上一次被动更新的时刻 */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Instant updateTime;

    /** 上一次被动更新记录的用户 */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private String updateBy;

}
