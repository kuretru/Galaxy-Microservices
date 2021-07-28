package com.kuretru.api.common.controller;

import com.kuretru.api.common.entity.enums.BaseEnum;

import java.util.Map;
import java.util.TreeMap;

/**
 * 提供文档控制器的通用方法
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public abstract class BaseDocumentController extends BaseController {

    protected Map<BaseEnum<?>, String> buildEnumMap(BaseEnum<?>[] values) {
        Map<BaseEnum<?>, String> result = new TreeMap<>();
        for (BaseEnum<?> value : values) {
            result.put(value, value.getValue());
        }
        return result;
    }

}
