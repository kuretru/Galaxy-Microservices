package com.kuretru.microservices.web.configuration;

import com.kuretru.microservices.common.entity.enums.BaseEnum;
import com.kuretru.microservices.common.utils.EnumUtils;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.convert.converter.Converter;
import org.springframework.core.convert.converter.ConverterFactory;
import org.springframework.format.FormatterRegistry;
import org.springframework.util.StringUtils;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * Spring MVC配置
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Configuration
public class WebMvcConfiguration implements WebMvcConfigurer {

    @Override
    public void addFormatters(FormatterRegistry registry) {
        registry.addConverterFactory(new BaseEnumConverterFactory());
    }

    private static class BaseEnumConverterFactory implements ConverterFactory<String, BaseEnum<?>> {

        @Override
        public <T extends BaseEnum<?>> Converter<String, T> getConverter(Class<T> targetType) {
            return source -> {
                if (!StringUtils.hasText(source)) {
                    return null;
                }
                return convert(targetType, source);
            };
        }

        @SuppressWarnings({"unchecked", "rawtypes"})
        private static <T extends BaseEnum<?>> T convert(Class<T> targetType, String source) {
            return (T) EnumUtils.valueOf((Class) targetType, source);
        }

    }

}
