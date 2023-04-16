package com.kuretru.microservices.web.entity.mapper;

import com.kuretru.microservices.web.entity.data.BaseHistoryDO;
import com.kuretru.microservices.web.entity.transfer.BaseHistoryDTO;
import org.mapstruct.Mapping;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseHistoryEntityMapper<D extends BaseHistoryDO, T extends BaseHistoryDTO> {

    /**
     * 将数据传输实体转换为数据实体
     *
     * @param record 数据传输实体
     * @return 数据实体
     */
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "createTime", ignore = true)
    D dtoToDo(T record);

}
