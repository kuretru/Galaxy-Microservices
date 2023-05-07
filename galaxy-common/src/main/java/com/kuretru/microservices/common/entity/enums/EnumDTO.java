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

    private String label;

    private String value;

}
