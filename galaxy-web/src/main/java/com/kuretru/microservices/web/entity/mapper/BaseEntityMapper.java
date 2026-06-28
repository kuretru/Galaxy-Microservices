package com.kuretru.microservices.web.entity.mapper;

import com.kuretru.microservices.web.entity.data.BaseDO;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;

import java.util.List;

public interface BaseEntityMapper<D extends BaseDO, T extends BaseDTO> {

    /**
     * 将DO转换为DTO
     *
     * @param record DO
     * @return DTO
     */
    T doToDto(D record);

    /**
     * 将DO批量转换为DTO
     *
     * @param records DO列表
     * @return DTO列表
     */
    List<T> doToDto(List<D> records);

    /**
     * 将DTO转换为DO
     *
     * @param record DTO
     * @return DO
     */
    D dtoToDo(T record);

    /**
     * 将DTO批量转换为DO
     *
     * @param records DTO列表
     * @return DO列表
     */
    List<D> dtoToDo(List<T> records);

}
