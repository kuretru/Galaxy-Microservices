package com.kuretru.microservices.web.entity.data;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

@Data
public abstract class BaseDO {

    /** 物理主键，自增 */
    @TableId(type = IdType.AUTO)
    private Long id;

}
