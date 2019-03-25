package com.kuretru.api.common.controller;

import lombok.Getter;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Getter
public abstract class BaseController {

    protected HttpServletRequest request;
    protected HttpServletResponse response;
    protected HttpSession session;

    //TODO 这种写法会在单实例模式下可能会导致不对应的情况
    @ModelAttribute
    public void init(HttpServletRequest request, HttpServletResponse response) {
        this.request = request;
        this.response = response;
        this.session = request.getSession();
    }

}
