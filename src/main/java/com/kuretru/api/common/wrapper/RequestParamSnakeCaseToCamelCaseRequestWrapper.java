package com.kuretru.api.common.wrapper;

import com.kuretru.api.common.util.CaseUtils;
import com.kuretru.api.common.util.HashMapUtils;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class RequestParamSnakeCaseToCamelCaseRequestWrapper extends HttpServletRequestWrapper {

    Map<String, String[]> parameterMap;

    public RequestParamSnakeCaseToCamelCaseRequestWrapper(HttpServletRequest request) {
        super(request);
        Map<String, String[]> oldMap = request.getParameterMap();
        this.parameterMap = new HashMap<>(HashMapUtils.initialCapacity(oldMap.size()));
        oldMap.forEach((k, v) -> {
            if (k.contains(CaseUtils.SNAKE_STRING)) {
                k = CaseUtils.snakeToCamel(k);
            }
            this.parameterMap.put(k, v);
        });
    }

    @Override
    public String getParameter(String name) {
        String[] values = this.parameterMap.get(name);
        if (values == null || values.length == 0) {
            return null;
        }
        return values[0];
    }

    @Override
    public Map<String, String[]> getParameterMap() {
        return this.parameterMap;
    }

    @Override
    public Enumeration<String> getParameterNames() {
        return Collections.enumeration(this.parameterMap.keySet());
    }

    @Override
    public String[] getParameterValues(String name) {
        return this.parameterMap.get(name);
    }

}
