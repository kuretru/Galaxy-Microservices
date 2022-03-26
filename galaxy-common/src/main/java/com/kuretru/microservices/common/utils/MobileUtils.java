package com.kuretru.microservices.common.utils;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class MobileUtils {

    /** 手机号码脱敏匹配正则表达式 */
    public static final String PHONE_BLUR_REGEX = "(\\d{3})\\d{4}(\\d{4})";

    /** 手机号码脱敏替换正则表达式 */
    public static final String PHONE_BLUR_REPLACE_REGEX = "$1****$2";

    private MobileUtils() {

    }

    /**
     * 手机号码脱敏处理
     *
     * @param mobile 原始手机号码
     * @return 脱敏后的手机号码
     */
    public static String blurMobile(String mobile) {
        return mobile.replaceAll(PHONE_BLUR_REGEX, PHONE_BLUR_REPLACE_REGEX);
    }

}
