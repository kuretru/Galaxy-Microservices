package com.kuretru.microservices.common.entity.enums;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class EnumDTO {

    /** 前端展示的值 */
    private String label;

    /** 前端保存的值 */
    private String value;

}
