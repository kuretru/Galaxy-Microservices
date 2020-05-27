package com.kuretru.api.common.entity.enums;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public interface BaseEnum {

    Integer ERROR = -1;

    /**
     * 枚举的内部编号
     *
     * @return 内部编号
     */
    int getCode();

    /**
     * 枚举表示的信息
     *
     * @return 信息
     */
    String getMessage();

}
