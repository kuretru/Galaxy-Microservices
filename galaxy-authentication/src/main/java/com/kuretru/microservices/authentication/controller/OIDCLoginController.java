package com.kuretru.microservices.authentication.controller;

import com.kuretru.microservices.authentication.property.AuthenticationProperty;
import com.kuretru.microservices.web.controller.BaseController;
import com.kuretru.microservices.web.entity.ApiResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestClient;
import org.springframework.web.servlet.view.RedirectView;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

@RestController
public class OIDCLoginController extends BaseController {

    private final AuthenticationProperty property;
    private final RestClient restClient;

    @Autowired
    public OIDCLoginController(AuthenticationProperty property) {
        this.property = property;
        this.restClient = RestClient.create();
    }

    @GetMapping("/login")
    public RedirectView login() {
        var redirectUri = request.getRequestURL().toString()
                .replace(request.getRequestURI(), request.getContextPath() + "/login/callback");
        var url = "https://sso.i5zhen.com/application/o/authorize/"
                + "?response_type=code"
                + "&client_id=" + property.getCloudSSOClientId()
                + "&redirect_uri=" + URLEncoder.encode(redirectUri, StandardCharsets.UTF_8)
                + "&scope=openid%20profile%20email"
                + "&state=" + UUID.randomUUID();
        return new RedirectView(url);
    }

    @GetMapping("/login/callback")
    public ApiResponse<?> loginCallback(@RequestParam String code) {
        var redirectUri = request.getRequestURL().toString()
                .replace(request.getRequestURI(), request.getContextPath() + "/api/ping");

        var form = new LinkedMultiValueMap<String, String>();
        form.add("grant_type", "authorization_code");
        form.add("code", code);
        form.add("redirect_uri", redirectUri);
        form.add("client_id", property.getCloudSSOClientId());

        var result = restClient.post()
                .uri("https://sso.i5zhen.com/application/o/token/")
                .contentType(MediaType.APPLICATION_FORM_URLENCODED)
                .body(form)
                .retrieve()
                .body(Object.class);
        return ApiResponse.success(result);
    }

}
