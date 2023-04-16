package com.kuretru.microservices.oauth2.client.system.entity.mapper;

import com.kuretru.microservices.oauth2.client.system.entity.UserDO;
import com.kuretru.microservices.oauth2.client.system.entity.UserDTO;
import com.kuretru.microservices.web.entity.mapper.BaseEntityMapper;
import org.mapstruct.Mapper;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Mapper(componentModel = "spring")
public interface UserEntityMapper extends BaseEntityMapper<UserDO, UserDTO> {

}
