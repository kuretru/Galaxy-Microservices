package com.kuretru.microservices.oauth2.client.memory;

import com.kuretru.microservices.oauth2.client.entity.OAuth2AccessTokenDO;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AccessTokenDTO;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface OAuth2AccessTokenMemory {

    /**
     * 保存用户的OAuth2 AccessToken凭据
     *
     * @param id     服务端侧的用户ID
     * @param record AccessToken信息
     */
    void save(String id, OAuth2AccessTokenDTO.Response record);

    /**
     * 获取用户的OAuth2 AccessToken凭据
     *
     * @param id 服务端侧的用户ID
     * @return AccessToken信息
     */
    OAuth2AccessTokenDO get(String id);

    /**
     * 删除保存的用户的OAuth2 AccessToken凭据
     *
     * @param id 服务端侧的用户ID
     */
    void delete(String id);

}
