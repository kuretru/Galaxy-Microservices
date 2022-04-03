package com.kuretru.microservices.oauth2.server.memory;

import com.kuretru.microservices.oauth2.common.entity.OAuth2AuthorizeDTO;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface OAuth2UniqueTokenMemory {

    /**
     * 生成并保存返回给前端的一次性凭证
     *
     * @param record OAuth2认证请求实体
     * @return 一次性凭证
     */
    String generateAndSave(OAuth2AuthorizeDTO.Request record);

    /**
     * 查询并返回OAuth2认证请求实体
     *
     * @param token 一次性凭证
     * @return OAuth2认证请求实体
     */
    OAuth2AuthorizeDTO.Request getAndDelete(String token);

}
