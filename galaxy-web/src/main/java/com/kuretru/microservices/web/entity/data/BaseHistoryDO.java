package com.kuretru.microservices.web.entity.data;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.time.Instant;

/**
 * 历史数据对象父抽象类
 * 历史表仅用作统计分析，大部分情况不会修改，因此没有updateTime字段
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public abstract class BaseHistoryDO {

    /** 物理主键，自增 */
    @TableId(type = IdType.AUTO)
    private Long id;

    /** 记录主动创建的时刻 */
    private Instant createTime;

}
