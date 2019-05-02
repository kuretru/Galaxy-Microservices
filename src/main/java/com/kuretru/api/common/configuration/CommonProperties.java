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

    /**
     * 用于Redis键的公共前缀
     */
    @Value("${:COMMON}")
    private String projectPrefix;

    /**
     * 上传文件时后缀名的黑名单
     */
    @Value("${file.upload.suffix.blacklist:jsp|asp|aspx|php|java|class}")
    private String fileUploadSuffixBlacklist;

    /**
     * 上传文件时后缀名的白名单
     */
    @Value("${file.upload.suffix.whitelist:jpg|png|jpeg}")
    private String fileUploadSuffixWhitelist;

    /**
     * 上传文件时后缀名的默认策略
     */
    @Value("${file.upload.suffix.default-policy:true}")
    private Boolean fileUploadSuffixDefaultPolicy;

    /**
     * 一次上传文件最大个数
     */
    @Value("${file.upload.count:9}")
    private Integer fileUploadCount;

    /**
     * 上传文件的根目录
     */
    @Value("${file.upload.root:}")
    private String fileUploadRoot;

    /**
     * 拼接文件下载目录时的前缀
     */
    @Value("${file.cdn.prefix:}")
    private String fileCdnPrefix;

    /**
     * 拼接文件下载目录时的后缀
     */
    @Value("${file.cdn.suffix:}")
    private String fileCdnSuffix;

}
