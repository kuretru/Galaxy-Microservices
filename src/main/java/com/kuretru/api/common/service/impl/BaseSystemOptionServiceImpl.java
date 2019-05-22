package com.kuretru.api.common.service.impl;

import com.kuretru.api.common.entity.data.SystemOptionDO;
import com.kuretru.api.common.entity.data.SystemOptionDTO;
import com.kuretru.api.common.mapper.SystemOptionMapper;
import com.kuretru.api.common.service.SystemOptionService;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public abstract class BaseSystemOptionServiceImpl extends BaseServiceImpl<SystemOptionMapper, SystemOptionDO, SystemOptionDTO> implements SystemOptionService {

    public BaseSystemOptionServiceImpl(SystemOptionMapper mapper) {
        super(mapper, SystemOptionDO.class, SystemOptionDTO.class);
    }

}
