package com.kuretru.microservices.web.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.kuretru.microservices.web.entity.enums.OAuth2ErrorEnum;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotNull;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class OAuth2ErrorResponse {

    @NotNull
    private OAuth2ErrorEnum<?> error;

    @JsonProperty("error_description")
    private String errorDescription;

    @JsonProperty("error_uri")
    private String errorUri;

    public OAuth2ErrorResponse(OAuth2ErrorEnum<?> error) {
        this.error = error;
        this.errorDescription = error.getValue();
    }

    public OAuth2ErrorResponse(OAuth2ErrorEnum<?> error, String errorDescription) {
        this.error = error;
        this.errorDescription = errorDescription;
    }

}
