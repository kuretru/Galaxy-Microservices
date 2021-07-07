package com.kuretru.api.common.entity;

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
public class PaginationResponse<T> {

    private List<T> list;

    private Long current;

    private Long pageSize;

    private Long total;

}
