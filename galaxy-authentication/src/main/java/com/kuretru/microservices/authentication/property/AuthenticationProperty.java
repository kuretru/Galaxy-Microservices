package com.kuretru.microservices.authentication.property;

import lombok.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.boot.context.properties.bind.DefaultValue;

@Value
@ConfigurationProperties(prefix = "galaxy.authentication")
public class AuthenticationProperty {

    String CloudSSOClientId;

    public AuthenticationProperty(@DefaultValue("U9XLAUhQIiyfwscUfdQwRmnBJvfTIbcAxXuMZJYa") String cloudSSOClientId) {
        this.CloudSSOClientId = cloudSSOClientId;
    }

}
