package com.kuretru.api.common.util;

import com.kuretru.api.common.entity.ApiResponse;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class SerializationUtilsTest {

    private static final String JSON = "{\"code\":2000,\"message\":\"success\",\"data\":\"成功\"}";

    @Test
    public void toJson() {
        ApiResponse data = ApiResponse.success("成功");
        String json = SerializationUtils.toJson(data);
        assertEquals(JSON, json);
    }

    @Test
    public void fromJson() {
        ApiResponse data = SerializationUtils.fromJson(JSON, ApiResponse.class);
        assertEquals(ApiResponse.SUCCESS, data.getCode());
        assertEquals("success", data.getMessage());
        assertEquals("成功", data.getData());
    }

}