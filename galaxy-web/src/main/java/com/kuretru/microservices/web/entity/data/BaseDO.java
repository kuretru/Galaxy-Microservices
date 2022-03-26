package com.kuretru.microservices.web.entity.data;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.time.Instant;

/**
 * 数据对象父抽象类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public abstract class BaseDO {

    /** 物理主键，自增 */
    @TableId(type = IdType.AUTO)
    private Long id;

    /** 业务逻辑主键 */
    private String uuid;

    /** 记录主动创建的时刻 */
    private Instant createTime;

    /** 记录上一次被动更新的时刻 */
    private Instant updateTime;

}
