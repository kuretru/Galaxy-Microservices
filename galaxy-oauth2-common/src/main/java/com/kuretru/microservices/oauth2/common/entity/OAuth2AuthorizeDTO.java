package com.kuretru.microservices.oauth2.common.entity;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * OAuth2 认证阶段的传输实体
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class OAuth2AuthorizeDTO {

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    @JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
    public static class Request implements Serializable {

        @Setter
        @NotNull
        private String responseType;

        @NotNull
        private String clientId;

        @JsonInclude(JsonInclude.Include.NON_EMPTY)
        private String redirectUri;

        @JsonInclude(JsonInclude.Include.NON_EMPTY)
        private String scope;

        @NotNull
        private String state;

    }

    @Data
    public static class Response {

        @NotNull
        private String code;

        @NotNull
        private String state;

    }

}
