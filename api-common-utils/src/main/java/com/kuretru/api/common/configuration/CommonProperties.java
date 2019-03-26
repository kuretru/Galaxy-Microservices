package com.kuretru.api.common.configuration;

import lombok.Data;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@Component
@EnableConfigurationProperties(CommonProperties.class)
@ConfigurationProperties(prefix = "kuretru.common")
public class CommonProperties {

    @Value("${:COMMON}")
    private String projectPrefix;

}
