package com.kuretru.microservices.oauth2.client.system.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.microservices.oauth2.client.system.entity.UserDO;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Mapper
@Repository
public interface UserMapper extends BaseMapper<UserDO> {

}
