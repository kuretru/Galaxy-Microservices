package com.kuretru.microservices.authentication.manager;

import com.kuretru.microservices.authentication.entity.AccessTokenBO;
import com.kuretru.microservices.authentication.entity.AccessTokenDTO;
import com.kuretru.microservices.web.exception.ServiceException;

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
     * @param roles  该用户拥有的角色列表
     * @return AccessToken
     */
    AccessTokenDTO generate(UUID userId, Set<String> roles);

    /**
     * 根据AccessTokenId查询AccessToken
     *
     * @param id AccessTokenId
     * @return AccessToken
     * @throws ServiceException 找不到指定AccessToken时或AccessToken已过期时，引发业务异常
     */
    AccessTokenBO get(String id) throws ServiceException;

    /**
     * 刷新AccessToken的过期时间
     *
     * @param id AccessTokenId
     * @throws ServiceException 找不到指定AccessToken时引发业务异常
     */
    void refresh(String id) throws ServiceException;

    /**
     * 吊销指定AccessToken，并从数据库中删除
     *
     * @param id AccessTokenId
     * @throws ServiceException 找不到指定AccessToken时引发业务异常
     */
    void revoke(String id) throws ServiceException;

}
