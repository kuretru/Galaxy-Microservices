package com.kuretru.microservices.web.entity;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 分页查询参数实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "分页查询查询条件")
public class PaginationQuery {

    @Schema(description = "当前页码")
    private Long current;

    @Schema(description = "每页记录条数")
    private Long pageSize;

    @Schema(description = "强制不分页")
    private Boolean noPage;

    public static boolean isNull(PaginationQuery paginationQuery) {
        return paginationQuery == null || paginationQuery.current == null || paginationQuery.pageSize == null;
    }

    public static boolean isNotNull(PaginationQuery paginationQuery) {
        return !isNull(paginationQuery);
    }

}
