package com.kuretru.microservices.web.service;

import com.kuretru.microservices.web.entity.data.BaseDO;
import com.kuretru.microservices.web.entity.transfer.BaseDTO;
import com.kuretru.microservices.web.service.impl.BaseServiceImpl;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.ToString;
import org.junit.jupiter.api.Test;
import org.mapstruct.Mapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.time.Instant;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@SpringBootTest
class EntityMapperTest {

    private final PersonEntityMapper entityMap;

    @Autowired
    public EntityMapperTest(PersonEntityMapper entityMap) {
        this.entityMap = entityMap;
    }

    @Test
    void doToDto() {
        PersonDO personDO = new PersonDO();
        personDO.setId(1L);
        personDO.setUuid(UUID.randomUUID().toString());
        personDO.setCreateTime(Instant.now());
        personDO.setUpdateTime(personDO.getCreateTime());
        personDO.setName("张三");
        personDO.setMobile(12345678);
        personDO.setSocial(UUID.randomUUID().toString());

        PersonDTO personDTO = entityMap.doToDto(personDO);
        assertEquals(personDO.getUuid(), personDTO.getId().toString());
        assertEquals(personDO.getName(), personDTO.getName());
        assertEquals(personDO.getMobile(), personDTO.getMobile());
        assertEquals(personDO.getSocial(), personDTO.getSocial().toString());
    }

    @Test
    void dtoToDo() {
        PersonDTO personDTO = new PersonDTO();
        personDTO.setId(UUID.randomUUID());
        personDTO.setName("张三");
        personDTO.setMobile(12345678);
        personDTO.setSocial(UUID.randomUUID());

        PersonDO personDO = entityMap.dtoToDo(personDTO);
        assertEquals(personDTO.getId(), UUID.fromString(personDO.getUuid()));
        assertEquals(personDTO.getName(), personDO.getName());
        assertEquals(personDTO.getSocial(), UUID.fromString(personDO.getSocial()));
        assertNull(personDO.getId());
        assertNull(personDO.getCreateTime());
        assertNull(personDO.getUpdateTime());
    }

    @Mapper(componentModel = "spring")
    interface PersonEntityMapper extends BaseServiceImpl.BaseEntityMapper<PersonDO, PersonDTO> {

    }

    @Data
    @EqualsAndHashCode(callSuper = true)
    @ToString(callSuper = true)
    static class PersonDO extends BaseDO {

        private String name;
        private Integer mobile;
        private String social;

    }

    @Data
    @EqualsAndHashCode(callSuper = true)
    @ToString(callSuper = true)
    static class PersonDTO extends BaseDTO {

        private String name;
        private Integer mobile;
        private UUID social;

    }


}
