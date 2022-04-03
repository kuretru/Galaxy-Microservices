package com.kuretru.microservices.oauth2.server.memory;

import com.kuretru.microservices.oauth2.server.entity.OAuth2ApprovedBO;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface OAuth2UniqueCodeMemory {

    /**
     * 生成并保存返回给客户端的一次性凭证
     *
     * @param record 授权实体
     * @return 一次性凭证
     */
    String generateAndSave(OAuth2ApprovedBO record);

    /**
     * 查询并返回授权实体
     *
     * @param code 一次性凭证
     * @return 授权实体
     */
    OAuth2ApprovedBO getAndDelete(String code);

}
