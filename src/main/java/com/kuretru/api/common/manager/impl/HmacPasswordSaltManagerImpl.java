package com.kuretru.api.common.manager.impl;

import com.kuretru.api.common.manager.PasswordSaltManager;
import com.kuretru.api.common.util.HashUtils;
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
