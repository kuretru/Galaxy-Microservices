package com.kuretru.microservices.web.entity.mapper;

import com.kuretru.microservices.web.entity.data.BaseSequenceDO;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import org.mapstruct.Mapping;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseSequenceEntityMapper<D extends BaseSequenceDO, T extends BaseDTO>
        extends BaseEntityMapper<D, T> {

    /**
     * 将数据传输实体转换为数据实体
     *
     * @param record 数据传输实体
     * @return 数据实体
     */
    @Mapping(source = "id", target = "uuid")
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "createTime", ignore = true)
    @Mapping(target = "updateTime", ignore = true)
    @Mapping(target = "sequence", ignore = true)
    D dtoToDo(T record);

}
