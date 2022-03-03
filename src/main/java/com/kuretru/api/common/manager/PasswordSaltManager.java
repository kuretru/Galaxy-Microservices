package com.kuretru.api.common.manager;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface PasswordSaltManager {

    /**
     * 随机生成一个盐
     *
     * @return 盐
     */
    String generateSalt();

    /**
     * 将密码混入盐
     *
     * @param message 密码
     * @param salt    盐
     * @return 混合盐后的密码
     */
    String mixSalt(final String message, final String salt);

}
