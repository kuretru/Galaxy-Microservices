package com.kuretru.api.common.controller;

import com.kuretru.api.common.annotation.RequestAuthorization;
import com.kuretru.api.common.entity.ApiResponse;
import com.kuretru.api.common.entity.transfer.BaseDTO;
import com.kuretru.api.common.exception.ApiException;
import com.kuretru.api.common.service.BaseService;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public abstract class BaseRestController<S extends BaseService<?, ?, T>, T extends BaseDTO> extends BaseCrudController<S, T> {

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
        super(service);
    }

    @RequestAuthorization
    @GetMapping("/{id}")
    @Override
    public ApiResponse get(@PathVariable("id") Long id) throws ApiException {
        return super.get(id);
    }

    @RequestAuthorization
    @GetMapping
    @Override
    public ApiResponse list() throws ApiException {
        return super.list();
    }

    @RequestAuthorization
    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    @Override
    public ApiResponse create(@RequestBody T record) throws ApiException {
        return super.create(record);
    }

    @RequestAuthorization
    @PutMapping("/{id}")
    @Override
    public ApiResponse update(@PathVariable("id") Long id, @RequestBody T record) throws ApiException {
        return super.update(id, record);
    }

    @RequestAuthorization
    @DeleteMapping("/{id}")
    @Override
    public ApiResponse remove(@PathVariable("id") Long id) throws ApiException {
        return super.remove(id);
    }

}
