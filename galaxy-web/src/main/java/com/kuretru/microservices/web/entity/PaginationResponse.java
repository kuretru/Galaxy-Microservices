package com.kuretru.microservices.web.entity;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 分页查询响应实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "分页查询响应实体")
public class PaginationResponse<T> {

    @Schema(description = "当页记录列表")
    private List<T> list;

    @Schema(description = "当前页码")
    private Long current;

    @Schema(description = "每页记录条数")
    private Long pageSize;

    @Schema(description = "记录总数")
    private Long total;

}
