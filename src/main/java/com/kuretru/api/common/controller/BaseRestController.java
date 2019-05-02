package com.kuretru.api.common.controller;

import com.kuretru.api.common.annotation.RequestAuthorization;
import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ApiException;
import com.kuretru.api.common.exception.NotFoundException;
import com.kuretru.api.common.service.BaseService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public abstract class BaseRestController<S extends BaseService<?, ?, T>, T extends BaseDTO> extends BaseController {

    protected S service;

    /**
     * 子类中必须调用此构造函数，传递参数
     * <pre>
     * @RestController
     * @RequestMapping(value = "/api/tags", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
     * public class WebTagController extends BaseRestController<WebTagService, WebTagDTO> {
     *
     *     @Autowired
     *     public WebTagController(WebTagService service) {
     *         super(service);
     *     }
     *
     * }
     * </pre>
     *
     * @param service 对应的Service
     */
    public BaseRestController(S service) {
        this.service = service;
    }

    @RequestAuthorization
    @GetMapping("/{id}")
    public ApiResponse get(@PathVariable("id") Long id) throws ApiException {
        T result = service.get(id);
        if (result == null) {
            throw new NotFoundException("未找到相关对象！");
        }
        return ApiResponse.success(result);
    }

    @RequestAuthorization
    @GetMapping
    public ApiResponse list() throws ApiException {
        List<T> result = service.list();
        if (result.isEmpty()) {
            throw new NotFoundException("未找到相关对象！");
        }
        return ApiResponse.success(result);
    }

    @RequestAuthorization
    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public ApiResponse create(@RequestBody T record) throws ApiException {
        T result = service.save(record);
        return ApiResponse.created(result);
    }

    @RequestAuthorization
    @PutMapping("/{id}")
    public ApiResponse update(@PathVariable("id") Long id, @RequestBody T record) throws ApiException {
        record.setId(id);
        T result = service.update(record);
        return ApiResponse.updated(result);
    }

    @RequestAuthorization
    @DeleteMapping("/{id}")
    public ApiResponse remove(@PathVariable("id") Long id) throws ApiException {
        int result = service.remove(id);
        if (1 != result) {
            throw new NotFoundException("未找到相关对象！");
        }
        return ApiResponse.removed();
    }

}
