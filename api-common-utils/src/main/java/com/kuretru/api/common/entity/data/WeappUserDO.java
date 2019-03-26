package com.kuretru.api.common.entity.data;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.Instant;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("weapp_user")
public class WeappUserDO extends BaseDO {

    private String openId;

    private String unionId;

    private String nickName;

    private String avatarUrl;

    private Integer gender;

    private String country;

    private String province;

    private String city;

    private Instant lastLogin;

    @Override
    public void addCrateTime() {
        super.addCrateTime();
        this.lastLogin = Instant.now();
    }

}
