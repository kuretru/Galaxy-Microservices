package com.kuretru.api.common.configuration;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "kuretru.common")
public class CommonProperties {

    private String projectPrefix;

}
