package com.kuretru.api.common.exception;

import com.kuretru.api.common.constant.ResponseCodes;
import com.kuretru.api.common.constant.ServiceErrorCodes;
import com.kuretru.api.common.constant.UserErrorCodes;
import lombok.Getter;

/**
 * 业务异常
 *
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class ServiceException extends Exception {

    @Getter
    private final ResponseCodes code;

    private ServiceException(ResponseCodes code, String message) {
        super(message);
        this.code = code;
    }

    public static final class BadRequest extends ServiceException {

        public BadRequest(UserErrorCodes code, String message) {
            super(code, message);
        }

    }

    public static final class Unauthorized extends ServiceException {

        public Unauthorized(UserErrorCodes code, String message) {
            super(code, message);
        }

    }

    public static final class Forbidden extends ServiceException {

        public Forbidden(UserErrorCodes code, String message) {
            super(code, message);
        }

    }

    public static final class NotFound extends ServiceException {

        public NotFound(UserErrorCodes code, String message) {
            super(code, message);
        }

    }


    public static final class InternalServerError extends ServiceException {

        public InternalServerError(ServiceErrorCodes code, String message) {
            super(code, message);
        }

    }

}
