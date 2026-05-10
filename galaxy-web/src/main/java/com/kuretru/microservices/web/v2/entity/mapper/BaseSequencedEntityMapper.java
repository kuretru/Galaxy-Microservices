package com.kuretru.microservices.web.v2.entity.mapper;

import com.kuretru.microservices.web.entity.interfaces.Sequenced;
import com.kuretru.microservices.web.v2.entity.data.BaseDO;
import com.kuretru.microservices.web.v2.entity.transfer.BaseDTO;
import org.mapstruct.Mapping;

public interface BaseSequencedEntityMapper<D extends BaseDO & Sequenced, T extends BaseDTO> extends BaseEntityMapper<D, T> {

    @Override
    @Mapping(target = "sequence", ignore = true)
    D dtoToDo(T record);

}
