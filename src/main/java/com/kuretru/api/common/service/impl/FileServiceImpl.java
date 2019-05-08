package com.kuretru.api.common.service.impl;

import com.kuretru.api.common.configuration.CommonProperties;
import com.kuretru.api.common.entity.transfer.FileUploadDTO;
import com.kuretru.api.common.exception.ApiException;
import com.kuretru.api.common.service.FileService;
import com.kuretru.api.common.util.InstantUtils;
import com.kuretru.api.common.util.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Service
public class FileServiceImpl implements FileService {

    private final List<String> blacklist;
    private final List<String> whitelist;
    private final Path rootPath;
    private CommonProperties commonProperties;

    @Autowired
    public FileServiceImpl(CommonProperties commonProperties) {
        this.commonProperties = commonProperties;
        this.blacklist = StringUtils.stringToList(commonProperties.getFileUploadSuffixBlacklist());
        this.whitelist = StringUtils.stringToList(commonProperties.getFileUploadSuffixWhitelist());
        this.rootPath = Paths.get(commonProperties.getFileUploadRoot())
                .resolve("images").toAbsolutePath().normalize();
    }

    @Override
    public List<FileUploadDTO> upload(List<MultipartFile> files) throws ApiException {
        // 检查文件数量
        if (files.size() > commonProperties.getFileUploadCount()) {
            throw new ApiException("超出一次性上传文件个数的限制");
        }
        // 检查后缀名
        List<FileUploadDTO> result = new ArrayList<>(files.size());
        for (MultipartFile file : files) {
            if (file.isEmpty()) {
                throw new ApiException("文件不存在");
            }
            if (StringUtils.isNullOrEmpty(file.getOriginalFilename())) {
                throw new ApiException("文件名不存在");
            }
            checkSuffix(file.getOriginalFilename());
        }
        // 合法文件则开始写入
        for (MultipartFile file : files) {
            // 新建文件名
            String time = InstantUtils.instantToString(Instant.now()).replace(" ", "_").replace(":", ";");
            String nonce = UUID.randomUUID().toString().substring(0, 6);
            String suffix = getFileSuffix(file.getOriginalFilename());
            String filename = time + "-" + nonce + "." + suffix;

            // 写入文件
            Path targetLocation = rootPath.resolve(filename);
            try {
                Files.copy(file.getInputStream(), targetLocation, StandardCopyOption.REPLACE_EXISTING);
            } catch (IOException e) {
                throw new ApiException(e.getMessage());
            }

            // 设置返回实体
            String url = commonProperties.getFileCdnPrefix() + "upload/" + filename + commonProperties.getFileCdnSuffix();
            FileUploadDTO dto = new FileUploadDTO(filename, url);
            result.add(dto);
        }
        return result;
    }

    @Override
    public void checkSuffix(String fileName) throws ApiException {
        String suffix = getFileSuffix(fileName);
        if (fileName.equals(suffix)) {
            throw new ApiException("文件没有后缀名");
        }
        // 1.匹配黑名单
        for (String black : blacklist) {
            if (black.equals(suffix)) {
                throw new ApiException("禁止上传敏感文件，文件后缀名在黑名单内");
            }
        }
        // 2.匹配默认策略
        if (commonProperties.getFileUploadSuffixDefaultPolicy()) {
            return;
        }
        // 3.匹配白名单
        for (String white : whitelist) {
            if (white.equals(suffix)) {
                return;
            }
        }
        // 4.默认禁止
        throw new ApiException("文件后缀名不在白名单内");
    }

    private String getFileSuffix(String fileName) {
        int lastDotIndex = fileName.lastIndexOf(".");
        return fileName.substring(lastDotIndex + 1);
    }

}
