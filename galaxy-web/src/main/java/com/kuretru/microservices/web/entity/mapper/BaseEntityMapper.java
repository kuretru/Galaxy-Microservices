package com.kuretru.microservices.web.entity.mapper;

import com.kuretru.microservices.web.entity.data.BaseDO;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import org.mapstruct.Mapping;

import java.util.List;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface BaseEntityMapper<D extends BaseDO, T extends BaseDTO> {

    /**
     * 将数据实体转换为数据传输实体
     *
     * @param record 数据实体
     * @return 数据传输实体
     */
    @Mapping(source = "uuid", target = "id")
    T doToDto(D record);

    /**
     * 将数据实体批量转换为数据传输实体
     *
     * @param records 数据实体列表
     * @return 数据传输实体列表
     */
    List<T> doToDto(List<D> records);

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
    D dtoToDo(T record);

    /**
     * 将数据传输实体批量转换为数据实体
     *
     * @param records 数据传输实体列表
     * @return 数据实体列表
     */
    List<D> dtoToDo(List<T> records);

}
