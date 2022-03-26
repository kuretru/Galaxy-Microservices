package com.kuretru.microservices.oauth2.common.entity;

import com.fasterxml.jackson.databind.PropertyNamingStrategies;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
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
@JsonNaming(PropertyNamingStrategies.SnakeCaseStrategy.class)
public class OAuth2ErrorResponse {

    @NotNull
    private OAuth2ErrorEnum error;

    private String errorDescription;

    private String errorUri;

    public OAuth2ErrorResponse(OAuth2ErrorEnum error) {
        this.error = error;
        this.errorDescription = error.getValue();
    }

    public OAuth2ErrorResponse(OAuth2ErrorEnum error, String errorDescription) {
        this.error = error;
        this.errorDescription = errorDescription;
    }

}
