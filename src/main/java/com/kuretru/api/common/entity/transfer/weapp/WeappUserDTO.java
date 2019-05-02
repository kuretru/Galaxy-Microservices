package com.kuretru.api.common.entity.transfer.weapp;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.kuretru.api.common.entity.enums.WeappGenderEnum;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.util.InstantUtils;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.time.Instant;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class WeappUserDTO extends BaseDTO {

    private String nickName;

    private String avatarUrl;

    private WeappGenderEnum gender;

    private String country;

    private String province;

    private String city;

    @JsonFormat(pattern = InstantUtils.GENERAL_DATE_FORMAT, timezone = InstantUtils.GENERAL_TIME_ZONE)
    private Instant lastLogin;

}
