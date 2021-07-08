package com.kuretru.api.common.entity;

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
public class PaginationQuery {

    /** 当前页数 */
    private Long current;

    /** 每页条数 */
    private Long pageSize;

    /**
     * 为了兼容下划线格式(page_size)传递的pageSize参数
     *
     * @param page_size 每页条数
     */
    public void setPage_size(Long page_size) {
        this.pageSize = page_size;
    }

}
