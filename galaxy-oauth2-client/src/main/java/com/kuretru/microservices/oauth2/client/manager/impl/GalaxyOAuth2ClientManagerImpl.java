package com.kuretru.microservices.oauth2.client.manager.impl;

import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.oauth2.client.entity.OAuth2AuthorizeRequestDTO;
import com.kuretru.microservices.oauth2.client.manager.OAuth2ClientManager;
import com.kuretru.microservices.oauth2.client.manager.OAuth2ClientStateManager;
import com.kuretru.microservices.oauth2.client.property.OAuth2ClientProperty;
import com.kuretru.microservices.oauth2.common.constant.OAuth2Constants;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AccessTokenDTO;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AuthorizeDTO;
import com.kuretru.microservices.web.constant.code.UserErrorCodes;
import com.kuretru.microservices.web.exception.ServiceException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.http.HttpEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
@ConditionalOnProperty("galaxy.oauth2.client.gemini.server-url")
public class GalaxyOAuth2ClientManagerImpl implements OAuth2ClientManager {

    private static final String AUTHORIZE_PATH = "/oauth2/authorize";
    private static final String ACCESS_TOKEN_PATH = "/oauth2/access_token";
    private final OAuth2ClientProperty property;
    private final OAuth2ClientStateManager stateManager;

    @Autowired
    public GalaxyOAuth2ClientManagerImpl(OAuth2ClientProperty property, OAuth2ClientStateManager stateManager) {
        this.property = property;
        this.stateManager = stateManager;
    }

    @Override
    public String authorize(OAuth2AuthorizeRequestDTO record) {
        String redirectUrl = StringUtils.nullToEmpty(record.getRedirectUri());
        OAuth2AuthorizeDTO.Request request = new OAuth2AuthorizeDTO.Request(
                OAuth2Constants.AUTHORIZATION_REQUEST_RESPONSE_TYPE,
                property.getGemini().getApplicationId(),
                redirectUrl,
                StringUtils.collectionToString(record.getScopes(), ","),
                stateManager.generateAndSave(redirectUrl)
        );
        return buildRedirectUrl(request);
    }

    @Override
    public OAuth2AccessTokenDTO.Response callback(OAuth2AuthorizeDTO.Response response) throws ServiceException {
        String redirectUrl = stateManager.getAndDelete(response.getState());
        if (redirectUrl == null) {
            throw ServiceException.build(UserErrorCodes.USER_LOGIN_EXPIRED, "OAuth2认证已过期，请重新认证");
        }
        return obtainAccessToken(response.getCode(), redirectUrl);
    }

    private String buildRedirectUrl(OAuth2AuthorizeDTO.Request request) {
        StringBuilder result = new StringBuilder()
                .append(property.getGemini().getServerUrl())
                .append(AUTHORIZE_PATH)
                .append("?response_type=code")
                .append("&client_id=").append(request.getClientId());
        if (org.springframework.util.StringUtils.hasText(request.getRedirectUri())) {
            result.append("&redirect_uri=").append(URLEncoder.encode(request.getRedirectUri(), StandardCharsets.UTF_8));
        }
        if (org.springframework.util.StringUtils.hasText(request.getScope())) {
            result.append("&scope=").append(request.getScope());
        }
        result.append("&state=").append(request.getState());
        return result.toString();
    }

    private OAuth2AccessTokenDTO.Response obtainAccessToken(String code, String redirectUrl) {
        OAuth2AccessTokenDTO.Request entity = new OAuth2AccessTokenDTO.Request(
                OAuth2Constants.ACCESS_TOKEN_REQUEST_GRANT_TYPE,
                code,
                redirectUrl,
                this.property.getGemini().getApplicationId(),
                this.property.getGemini().getApplicationSecret()
        );
        String url = this.property.getGemini().getServerUrl() + ACCESS_TOKEN_PATH;
        HttpEntity<OAuth2AccessTokenDTO.Request> request = new HttpEntity<>(entity);
        RestTemplate restTemplate = new RestTemplate();
        return restTemplate.postForObject(url, request, OAuth2AccessTokenDTO.Response.class);
    }

}
