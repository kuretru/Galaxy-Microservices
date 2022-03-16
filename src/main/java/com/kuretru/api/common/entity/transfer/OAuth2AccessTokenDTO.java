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
public class OAuth2AccessTokenDTO {

    @Getter
    public enum ErrorEnum {
        /** The request is missing a required parameter, includes an unsupported parameter value (other than grant type), repeats a parameter, includes multiple credentials, utilizes more than one mechanism for authenticating the client, or is otherwise malformed. */
        INVALID_REQUEST("invalid_request"),
        /** Client authentication failed */
        INVALID_CLIENT("invalid_client"),
        /** The provided authorization grant or refresh token is invalid, expired, revoked, does not match the redirection URI used in the authorization request, or was issued to another client. */
        INVALID_GRANT("invalid_grant"),
        /** The authenticated client is not authorized to use this authorization grant type. */
        UNAUTHORIZED_CLIENT("unauthorized_client"),
        /** The authorization grant type is not supported by the authorization server. */
        UNSUPPORTED_GRANT_TYPE("unsupported_grant_type"),
        /** The requested scope is invalid, unknown, malformed, or exceeds the scope granted by the resource owner. */
        INVALID_SCOPE("invalid_scope"),
        ;

        @JsonValue
        private final String value;

        ErrorEnum(String value) {
            this.value = value;
        }
    }

    @Data
    public static class Request {

        @JsonProperty("grant_type")
        @NotNull
        private String grantType;

        @NotNull
        private String code;

        @JsonProperty("redirect_uri")
        @JsonInclude(JsonInclude.Include.NON_EMPTY)
        @NotNull
        private String redirectUri;

        @JsonProperty("client_id")
        @NotNull
        private String clientId;

    }

    @Data
    public static class Response {

        @JsonProperty("access_token")
        @NotNull
        private String accessToken;

        @JsonProperty("token_type")
        @NotNull
        private String tokenType;

        @JsonProperty("expires_in")
        private Integer expiresIn;

        @JsonProperty("refresh_token")
        private String refreshToken;

        private String scope;

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
