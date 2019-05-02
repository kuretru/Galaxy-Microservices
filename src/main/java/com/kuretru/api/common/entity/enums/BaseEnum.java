package com.kuretru.api.common.entity.enums;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public interface BaseEnum {

    Integer ERROR = -1;

    int getCode();

    String getMessage();

}
