package com.kuretru.microservices.oauth2.client.manager.impl;

import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.oauth2.client.entity.OAuth2AuthorizeRequestDTO;
import com.kuretru.microservices.oauth2.client.manager.OAuth2ClientManager;
import com.kuretru.microservices.oauth2.client.manager.OAuth2ClientStateManager;
import com.kuretru.microservices.oauth2.client.property.OAuth2ClientProperty;
import com.kuretru.microservices.oauth2.common.constant.OAuth2Constants;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AuthorizeDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
@ConditionalOnProperty("galaxy.oauth2.client.gemini.server-url")
public class GalaxyOAuth2ClientManagerImpl implements OAuth2ClientManager {

    private static final String AUTHORIZE_PATH = "/oauth2/authorize";
    private final OAuth2ClientProperty property;
    private final OAuth2ClientStateManager stateManager;

    @Autowired
    public GalaxyOAuth2ClientManagerImpl(OAuth2ClientProperty property, OAuth2ClientStateManager stateManager) {
        this.property = property;
        this.stateManager = stateManager;
    }

    @Override
    public String authorize(OAuth2AuthorizeRequestDTO record) {
        String scope = StringUtils.collectionToString(record.getScopes(), ",");
        OAuth2AuthorizeDTO.Request request = new OAuth2AuthorizeDTO.Request(
                OAuth2Constants.AUTHORIZATION_CODE_GRANT,
                property.getGemini().getApplicationId(),
                record.getRedirectUri(),
                scope,
                stateManager.generate(scope)
        );
        return buildRedirectUrl(request);
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

}
