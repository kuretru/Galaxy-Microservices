package com.kuretru.api.common.entity.data;

import com.baomidou.mybatisplus.annotation.IdType;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;

import java.time.Instant;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
public abstract class BaseDO {

    @TableId(type = IdType.AUTO)
    private Long id;

    private Instant createAt;

    private Instant modifiedAt;

    public void addCrateTime() {
        Instant now = Instant.now();
        createAt = now;
        modifiedAt = now;
    }

}
