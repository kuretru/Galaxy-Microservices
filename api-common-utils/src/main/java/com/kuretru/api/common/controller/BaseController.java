package com.kuretru.api.common.controller;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
@Getter
public abstract class BaseController {

    @Autowired
    protected HttpServletRequest request;
    @Autowired
    protected HttpSession session;

}
