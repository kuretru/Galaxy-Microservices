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

    /** 当前页数 */
    @Schema(description = "当前页码")
    private Long current;

    /** 每页条数 */
    @Schema(description = "每页记录条数")
    private Long pageSize;

    public static boolean isNull(PaginationQuery paginationQuery) {
        return paginationQuery == null || paginationQuery.current == null || paginationQuery.pageSize == null;
    }

    public static boolean isNotNull(PaginationQuery paginationQuery) {
        return !isNull(paginationQuery);
    }

    /**
     * 为了兼容下划线格式(page_size)传递的pageSize参数
     *
     * @param pageSize 每页条数
     */
    public void setPage_size(Long pageSize) {
        this.pageSize = pageSize;
    }

}
