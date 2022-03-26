package com.kuretru.microservices.web.controller;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 * 基控制器，提供一些基本成员及方法
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Getter
public abstract class BaseController {

    @Autowired
    protected HttpServletRequest request;
    @Autowired
    protected HttpSession session;
    @Autowired
    protected HttpServletResponse response;

}
