package com.kuretru.microservices.web.entity.data;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.time.Instant;

/**
 * 关联表父抽象类
 * 关联表仅用于关联两个表，只有增、删、查情景，因此没有updateTime字段
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class BaseRelationDO {

    /** 物理主键，自增 */
    @TableId(type = IdType.AUTO)
    private Long id;

    /** 记录主动创建的时刻 */
    private Instant createTime;

}
