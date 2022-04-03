package com.kuretru.microservices.oauth2.server.manager;

import com.kuretru.microservices.common.utils.StringUtils;
import com.kuretru.microservices.oauth2.common.constant.OAuth2Constants;
import com.kuretru.microservices.oauth2.common.entity.OAuth2AccessTokenDTO;
import com.kuretru.microservices.oauth2.common.entity.OAuth2ErrorEnum;
import com.kuretru.microservices.oauth2.common.entity.OAuth2Triple;
import com.kuretru.microservices.oauth2.common.exception.OAuth2Exception;
import com.kuretru.microservices.oauth2.server.entity.OAuth2AccessTokenBO;
import com.kuretru.microservices.oauth2.server.entity.OAuth2AccessTokenDO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;

import java.io.Serializable;
import java.time.Duration;
import java.util.UUID;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
@Service
@ConditionalOnBean(RedisTemplate.class)
public class OAuth2AccessTokenManagerImpl implements OAuth2AccessTokenManager {

    private static final String REDIS_ROOT_KEY = "OAuth2AccessTokenManager.";
    private static final String REDIS_ACCESS_TOKEN_KEY = REDIS_ROOT_KEY + "accessToken.";
    private static final Duration ACCESS_TOKEN_EXPIRE_TIME = Duration.ofHours(2);

    private final RedisTemplate<String, Serializable> redisTemplate;

    @Autowired
    public OAuth2AccessTokenManagerImpl(RedisTemplate<String, Serializable> redisTemplate) {
        this.redisTemplate = redisTemplate;
    }

    @Override
    public OAuth2AccessTokenDTO.Response generate(OAuth2Triple triple) {
        String accessToken = StringUtils.randomUUID();
        while (Boolean.TRUE.equals(redisTemplate.hasKey(buildKey(accessToken)))) {
            accessToken = StringUtils.randomUUID();
        }

        OAuth2AccessTokenDO record = new OAuth2AccessTokenDO(triple);
        redisTemplate.opsForValue().set(buildKey(accessToken), record, ACCESS_TOKEN_EXPIRE_TIME);

        return new OAuth2AccessTokenDTO.Response(
                accessToken,
                OAuth2Constants.TOKEN_TYPE,
                System.currentTimeMillis() + ACCESS_TOKEN_EXPIRE_TIME.toSeconds(),
                record.getRefreshToken(),
                StringUtils.collectionToString(record.getScopes(), OAuth2Constants.SCOPES_SEPARATOR)
        );
    }

    @Override
    public UUID verify(String accessToken, String scope) throws OAuth2Exception {
        String key = buildKey(accessToken);
        exist(key);
        OAuth2AccessTokenDO record = (OAuth2AccessTokenDO)redisTemplate.opsForValue().get(key);
        assert record != null;
        if (!org.springframework.util.StringUtils.hasText(scope)) {
            return record.getUserId();
        } else if (record.getScopes().contains(scope)) {
            return record.getUserId();
        }
        throw new OAuth2Exception(OAuth2ErrorEnum.AccessTokenError.INVALID_SCOPE, "用户未授权此操作");
    }

    @Override
    public OAuth2AccessTokenBO refresh(OAuth2AccessTokenBO bo) throws OAuth2Exception {
        return null;
    }

    @Override
    public void revoke(String accessToken) throws OAuth2Exception {
        String key = buildKey(accessToken);
        exist(key);
        redisTemplate.delete(key);
    }

    private void exist(String key) throws OAuth2Exception {
        if (Boolean.FALSE.equals(redisTemplate.hasKey(key))) {
            throw new OAuth2Exception(OAuth2ErrorEnum.AccessTokenError.INVALID_GRANT, "AccessToken已过期");
        }
    }

    private String buildKey(String key) {
        return REDIS_ACCESS_TOKEN_KEY + key;
    }

}
