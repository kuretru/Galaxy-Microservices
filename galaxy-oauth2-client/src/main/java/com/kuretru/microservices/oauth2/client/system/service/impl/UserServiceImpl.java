package com.kuretru.microservices.oauth2.client.system.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.kuretru.microservices.authentication.context.AccessTokenContext;
import com.kuretru.microservices.authentication.entity.AccessTokenBO;
import com.kuretru.microservices.authentication.entity.AccessTokenDTO;
import com.kuretru.microservices.authentication.entity.UserLoginDTO;
import com.kuretru.microservices.authentication.manager.AccessTokenManager;
import com.kuretru.microservices.oauth2.client.system.entity.UserDO;
import com.kuretru.microservices.oauth2.client.system.entity.UserDTO;
import com.kuretru.microservices.oauth2.client.system.entity.UserQuery;
import com.kuretru.microservices.oauth2.client.system.mapper.UserMapper;
import com.kuretru.microservices.oauth2.client.system.service.UserService;
import com.kuretru.microservices.oauth2.common.entity.GalaxyUserDTO;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.exception.ServiceException;
import com.kuretru.microservices.web.service.impl.BaseServiceImpl;
import org.mapstruct.Mapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.Instant;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
public class UserServiceImpl extends BaseServiceImpl<UserMapper, UserDO, UserDTO, UserQuery> implements UserService {

    private final AccessTokenManager accessTokenManager;

    @Autowired
    public UserServiceImpl(UserMapper mapper, UserEntityMapper entityMapper, AccessTokenManager accessTokenManager) {
        super(mapper, entityMapper);
        this.accessTokenManager = accessTokenManager;
    }

    private UserDO getByGeminiId(UUID geminiId) {
        QueryWrapper<UserDO> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("gemini_id", geminiId.toString());
        return mapper.selectOne(queryWrapper);
    }

    @Override
    public UserLoginDTO login(GalaxyUserDTO record) throws ServiceException {
        UserDO userDo = getByGeminiId(record.getId());
        if (userDo == null) {
            throw new ServiceException(UserErrorCodes.WRONG_USERNAME, "该用户不存在");
        }

        userDo.setNickname(record.getNickname());
        userDo.setAvatar(record.getAvatar());
        userDo.setLastLogin(Instant.now());
        mapper.updateById(userDo);

        UUID userId = UUID.fromString(userDo.getUuid());
        AccessTokenDTO accessTokenDTO = accessTokenManager.generate(userId, null);
        return new UserLoginDTO(userId, accessTokenDTO);
    }

    @Override
    public void logout(String accessTokenId) throws ServiceException {
        AccessTokenBO accessTokenBO = accessTokenManager.get(accessTokenId);
        UUID userId = AccessTokenContext.getUserId();
        if (!accessTokenBO.getUserId().equals(userId)) {
            throw new ServiceException(UserErrorCodes.ACCESS_PERMISSION_ERROR, "AccessTokenID与操作用户不匹配");
        }
        accessTokenManager.revoke(accessTokenId);
    }

    @Mapper(componentModel = "spring")
    interface UserEntityMapper extends BaseServiceImpl.BaseEntityMapper<UserDO, UserDTO> {

    }

}
