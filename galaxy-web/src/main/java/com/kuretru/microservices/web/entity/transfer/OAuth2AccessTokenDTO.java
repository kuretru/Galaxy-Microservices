package com.kuretru.microservices.web.entity.transfer;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class OAuth2AccessTokenDTO {

    @Data
    @JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
    public static class Request {

        @NotNull
        private String grantType;

        @NotNull
        private String code;

        @NotNull
        @JsonInclude(JsonInclude.Include.NON_EMPTY)
        private String redirectUri;

        @NotNull
        private String clientId;

        @NotNull
        private String clientSecret;

    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
    public static class Response {

        @NotNull
        private String accessToken;

        @NotNull
        private String tokenType;

        private Integer expiresIn;

        private String refreshToken;

        private String scope;

    }

}
