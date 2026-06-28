package com.kuretru.microservices.web.configuration;

import com.kuretru.microservices.web.entity.enums.GenderEnum;
import org.junit.jupiter.api.Test;
import org.springframework.format.support.DefaultFormattingConversionService;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class WebMvcConfigurationTest {

    private final DefaultFormattingConversionService conversionService;

    WebMvcConfigurationTest() {
        conversionService = new DefaultFormattingConversionService();
        new WebMvcConfiguration().addFormatters(conversionService);
    }

    @Test
    void convertBaseEnumFromValue() {
        var result = conversionService.convert("male", GenderEnum.class);

        assertEquals(GenderEnum.MALE, result);
    }

    @Test
    void convertBlankBaseEnumToNull() {
        var result = conversionService.convert("", GenderEnum.class);

        assertNull(result);
    }

}
