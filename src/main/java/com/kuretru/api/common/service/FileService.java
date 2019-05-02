package com.kuretru.api.common.service;

import com.kuretru.api.common.entity.transfer.FileUploadDTO;
import com.kuretru.api.common.exception.ApiException;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public interface FileService {

    /**
     * 上传文件服务接口
     *
     * @param files 上传的文件
     * @return 文件传输实体
     * @throws ApiException 业务异常
     */
    List<FileUploadDTO> upload(List<MultipartFile> files) throws ApiException;

    /**
     * 检查上传的文件名是否合法
     *
     * @param fileName 文件名
     * @throws ApiException 业务异常
     */
    void checkSuffix(String fileName) throws ApiException;

}
