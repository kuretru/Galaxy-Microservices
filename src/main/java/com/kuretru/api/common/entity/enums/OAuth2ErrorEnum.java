package com.kuretru.api.common.entity.enums;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.Getter;
import org.springframework.http.HttpStatus;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public interface OAuth2ErrorEnum<T> {

    /**
     * 获取错误内容
     *
     * @return 错误内容
     */
    String getValue();

    /**
     * 发送该错误时应该返回的HTTP状态码
     *
     * @return HTTP状态码
     */
    HttpStatus getHttpStatus();

    @Getter
    enum AuthorizeError implements OAuth2ErrorEnum<AuthorizeError> {

        /** The request is missing a required parameter, includes an invalid parameter value, includes a parameter more than once, or is otherwise malformed. */
        INVALID_REQUEST("invalid_request", HttpStatus.BAD_REQUEST),
        /** The client is not authorized to request an authorization code using this method. */
        UNAUTHORIZED_CLIENT("unauthorized_client", HttpStatus.UNAUTHORIZED),
        /** The resource owner or authorization server denied the request. */
        ACCESS_DENIED("access_denied", HttpStatus.FORBIDDEN),
        /** The authorization server does not support obtaining an authorization code using this method. */
        UNSUPPORTED_RESPONSE_TYPE("unsupported_response_type", HttpStatus.BAD_REQUEST),
        /** The requested scope is invalid, unknown, or malformed. */
        INVALID_SCOPE("invalid_scope", HttpStatus.BAD_REQUEST),
        /** The authorization server encountered an unexpected condition that prevented it from fulfilling the request. */
        SERVER_ERROR("server_error", HttpStatus.INTERNAL_SERVER_ERROR),
        /** The authorization server is currently unable to handle the request due to a temporary overloading or maintenance of the server */
        TEMPORARILY_UNAVAILABLE("temporarily_unavailable", HttpStatus.SERVICE_UNAVAILABLE),
        ;

        @JsonValue
        private final String value;

        private final HttpStatus httpStatus;

        AuthorizeError(String value, HttpStatus httpStatus) {
            this.value = value;
            this.httpStatus = httpStatus;
        }
    }

    @Getter
    enum AccessTokenError implements OAuth2ErrorEnum<AccessTokenError> {

        /** The request is missing a required parameter, includes an unsupported parameter value (other than grant type), repeats a parameter, includes multiple credentials, utilizes more than one mechanism for authenticating the client, or is otherwise malformed. */
        INVALID_REQUEST("invalid_request", HttpStatus.BAD_REQUEST),
        /** Client authentication failed */
        INVALID_CLIENT("invalid_client", HttpStatus.UNAUTHORIZED),
        /** The provided authorization grant or refresh token is invalid, expired, revoked, does not match the redirection URI used in the authorization request, or was issued to another client. */
        INVALID_GRANT("invalid_grant", HttpStatus.BAD_REQUEST),
        /** The authenticated client is not authorized to use this authorization grant type. */
        UNAUTHORIZED_CLIENT("unauthorized_client", HttpStatus.UNAUTHORIZED),
        /** The authorization grant type is not supported by the authorization server. */
        UNSUPPORTED_GRANT_TYPE("unsupported_grant_type", HttpStatus.BAD_REQUEST),
        /** The requested scope is invalid, unknown, malformed, or exceeds the scope granted by the resource owner. */
        INVALID_SCOPE("invalid_scope", HttpStatus.UNAUTHORIZED),
        ;

        @JsonValue
        private final String value;

        private final HttpStatus httpStatus;

        AccessTokenError(String value, HttpStatus httpStatus) {
            this.value = value;
            this.httpStatus = httpStatus;
        }
    }

}
