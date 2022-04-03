package com.kuretru.microservices.oauth2.server.entity;

import com.fasterxml.jackson.annotation.JsonValue;
import lombok.*;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Data
public class OAuth2ApproveDTO {

    public enum Action {
        /** 同意授权 */
        APPROVE("approve"),
        /** 拒绝 */
        REJECT("reject");

        @JsonValue
        private final String value;

        Action(String value) {
            this.value = value;
        }

    }

    @Data
    @EqualsAndHashCode(callSuper = true)
    @ToString(callSuper = true)
    public static class Request extends OAuth2ApproveQuery {

        /** 授权动作 */
        @NotNull
        private Action action;

    }

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Response {

        /** 应用ID */
        @NotNull
        private UUID applicationId;

        /** 批准Token */
        @NotEmpty
        private String token;

        /** 授权的范围 */
        @NotEmpty
        private String scope;

    }

}
