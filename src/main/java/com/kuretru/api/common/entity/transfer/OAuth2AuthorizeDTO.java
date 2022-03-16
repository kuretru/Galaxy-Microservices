package com.kuretru.api.common.entity.transfer;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Data;
import lombok.Getter;

import javax.validation.constraints.NotNull;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class OAuth2AuthorizeDTO {

    @Getter
    public enum ErrorEnum {
        /** The request is missing a required parameter, includes an invalid parameter value, includes a parameter more than once, or is otherwise malformed. */
        INVALID_REQUEST("invalid_request"),
        /** The client is not authorized to request an authorization code using this method. */
        UNAUTHORIZED_CLIENT("unauthorized_client"),
        /** The resource owner or authorization server denied the request. */
        ACCESS_DENIED("access_denied"),
        /** The authorization server does not support obtaining an authorization code using this method. */
        UNSUPPORTED_RESPONSE_TYPE("unsupported_response_type"),
        /** The requested scope is invalid, unknown, or malformed. */
        INVALID_SCOPE("invalid_scope"),
        /** The authorization server encountered an unexpected condition that prevented it from fulfilling the request. */
        SERVER_ERROR("server_error"),
        /** The authorization server is currently unable to handle the request due to a temporary overloading or maintenance of the server */
        TEMPORARILY_UNAVAILABLE("temporarily_unavailable"),
        ;

        @JsonValue
        private final String value;

        ErrorEnum(String value) {
            this.value = value;
        }
    }

    @Data
    public static class Request {

        @JsonProperty("response_type")
        @NotNull
        private String responseType;

        @JsonProperty("client_id")
        @NotNull
        private String clientId;

        @JsonProperty("redirect_uri")
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

    @Data
    static class ErrorResponse {

        @NotNull
        private ErrorEnum error;

        @JsonProperty("error_description")
        private String errorDescription;

        @JsonProperty("error_uri")
        private String errorUri;

    }

}
