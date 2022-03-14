package com.kuretru.api.common.manager;

import com.kuretru.api.common.entity.business.AccessTokenBO;
import com.kuretru.api.common.entity.transfer.AccessTokenDTO;
import com.kuretru.api.common.exception.ServiceException;

import java.util.Set;
import java.util.UUID;

/**
 * AccessToken管理器
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface AccessTokenManager {

    /**
     * 生成一个AccessToken，并存入数据库
     *
     * @param userId 该AccessToken绑定的用户ID
     * @param roles  用户的角色列表
     * @return AccessToken
     */
    AccessTokenDTO generate(UUID userId, Set<String> roles);

    /**
     * 根据Token ID查询AccessToken
     *
     * @param id Token ID
     * @return AccessToken
     * @throws ServiceException 业务异常
     */
    AccessTokenBO get(String id) throws ServiceException;

    /**
     * 刷新AccessToken的过期时间
     *
     * @param id Token ID
     * @throws ServiceException 业务异常
     */
    void refresh(String id) throws ServiceException;

    /**
     * 吊销一个AccessToken，从数据库中删除
     *
     * @param id Token ID
     * @throws ServiceException 业务异常
     */
    void revoke(String id) throws ServiceException;

}
