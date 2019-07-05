package com.kuretru.api.common.controller;

import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ApiException;
import com.kuretru.api.common.exception.NotFoundException;
import com.kuretru.api.common.service.BaseService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

import java.util.List;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public abstract class BaseCrudController<S extends BaseService<?, ?, T>, T extends BaseDTO> extends BaseController {

    protected final S service;

    /**
     * 子类中必须调用此构造函数，传递参数
     * <pre>
     * @RestController
     * @RequestMapping(value = "/api/tags/{tagId}/categories", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
     * public class WebCategoryController extends BaseCrudController<WebCategoryService, WebCategoryDTO> {
     *
     *     @Autowired
     *     public WebCategoryController(WebCategoryService service) {
     *         super(service);
     *     }
     *
     * }
     * </pre>
     *
     * @param service 对应的Service
     */
    public BaseCrudController(S service) {
        this.service = service;
    }

    public ApiResponse get(Long id) throws ApiException {
        T result = service.get(id);
        if (result == null) {
            throw new NotFoundException("未找到相关对象！");
        }
        return ApiResponse.success(result);
    }

    public ApiResponse list() throws ApiException {
        List<T> result = service.list();
        if (result.isEmpty()) {
            throw new NotFoundException("未找到相关对象！");
        }
        return ApiResponse.success(result);
    }

    @ResponseStatus(HttpStatus.CREATED)
    public ApiResponse create(T record) throws ApiException {
        T result = service.save(record);
        return ApiResponse.created(result);
    }

    public ApiResponse update(Long id, T record) throws ApiException {
        record.setId(id);
        T result = service.update(record);
        return ApiResponse.updated(result);
    }

    public ApiResponse remove(Long id) throws ApiException {
        service.remove(id);
        return ApiResponse.removed();
    }

}
