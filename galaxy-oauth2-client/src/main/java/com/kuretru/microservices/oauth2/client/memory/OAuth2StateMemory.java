package com.kuretru.microservices.oauth2.client.memory;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface OAuth2StateMemory {

    /**
     * 生成一个state，并存入数据库
     *
     * @param redirectUri 重定向的URL
     * @return state
     */
    String generateAndSave(String redirectUri);

    /**
     * 根据state获取申请的权限范围，并从数据库中删除
     *
     * @param state state
     * @return 申请的权限范围
     */
    String getAndDelete(String state);

}
