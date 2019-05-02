package com.kuretru.api.common.entity.transfer;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class FileUploadDTO {

    private String fileName;

    private String url;

}
