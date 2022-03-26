package com.kuretru.microservices.web.constant;

import java.time.LocalDate;
import java.time.Month;
import java.util.UUID;

/**
 * 用于表示各类默认值或空值的常量类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class EmptyConstants {

    /** 空UUID值 */
    public static final UUID EMPTY_UUID = UUID.fromString("00000000-0000-0000-0000-000000000000");

    /** 空日期值 */
    public static final LocalDate EMPTY_DATE = LocalDate.of(2000, Month.JANUARY, 1);

    private EmptyConstants() {

    }

}
