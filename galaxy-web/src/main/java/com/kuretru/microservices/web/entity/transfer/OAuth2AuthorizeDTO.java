package com.kuretru.microservices.web.entity.transfer;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import lombok.Data;
import lombok.Setter;

import javax.validation.constraints.NotNull;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class OAuth2AuthorizeDTO {

    @Data
    @JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
    public static class Request {

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

        private String code;

        private String state;

        private String callback;

    }

}
