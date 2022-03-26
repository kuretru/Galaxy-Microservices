package com.kuretru.microservices.web.manager.impl;

import com.kuretru.microservices.common.utils.HashUtils;
import com.kuretru.microservices.web.manager.PasswordSaltManager;
import org.springframework.stereotype.Service;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
public class HmacPasswordSaltManagerImpl implements PasswordSaltManager {

    @Override
    public String generateSalt() {
        return HashUtils.generateSha256Key();
    }

    @Override
    public String mixSalt(String message, String salt) {
        return HashUtils.hmacSha256(message, salt);
    }

}
