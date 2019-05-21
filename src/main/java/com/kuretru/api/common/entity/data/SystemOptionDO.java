package com.kuretru.api.common.entity.data;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("sys_option")
@NoArgsConstructor
@AllArgsConstructor
public class SystemOptionDO extends BaseDO {

    private String key;

    private String value;

}
