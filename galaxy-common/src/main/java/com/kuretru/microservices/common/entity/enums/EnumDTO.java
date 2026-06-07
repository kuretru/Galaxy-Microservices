package com.kuretru.microservices.common.entity.enums;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
public class EnumDTO<T> {

    /** 前端展示的值 */
    private String label;

    /** 前端保存的值 */
    private T value;

    /** 子元素 */
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<EnumDTO<T>> children;

    public EnumDTO(String label, T value) {
        this.label = label;
        this.value = value;
    }

}
