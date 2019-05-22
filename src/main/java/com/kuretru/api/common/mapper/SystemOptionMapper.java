package com.kuretru.api.common.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.kuretru.api.common.entity.data.SystemOptionDO;
import org.apache.ibatis.annotations.Mapper;
import org.springframework.stereotype.Repository;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Mapper
@Repository
public interface SystemOptionMapper extends BaseMapper<SystemOptionDO> {

}
