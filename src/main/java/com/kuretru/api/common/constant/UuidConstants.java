package com.kuretru.api.common.constant;

import java.util.UUID;

/**
 * UUID相关的常量类
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class UuidConstants {

    /** 空UUID值，使用此值代替null */
    public static final UUID EMPTY = UUID.fromString("00000000-0000-0000-0000-000000000000");

    private UuidConstants() {

    }

}
