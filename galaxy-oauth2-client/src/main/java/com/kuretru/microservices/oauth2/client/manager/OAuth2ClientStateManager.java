package com.kuretru.microservices.oauth2.client.manager;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface OAuth2ClientStateManager {

    /**
     * 生成一个state，并存入数据库
     *
     * @param scope 申请的权限范围
     * @return state
     */
    String generate(String scope);

}
