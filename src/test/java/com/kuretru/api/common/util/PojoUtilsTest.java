package com.kuretru.api.common.util;

import com.kuretru.api.common.entity.data.SystemAdminDO;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class PojoUtilsTest {

    @Test
    public void toHashMap() {
        List<SystemAdminDO> records = new ArrayList<>(10);

        for (int i = 0; i < 10; ++i) {
            SystemAdminDO record = new SystemAdminDO();
            record.setId((long) i);
            records.add(record);
        }

        Map<Long, SystemAdminDO> map = PojoUtils.toHashMap(records);
        Assert.assertNotNull(map);
        for (int i = 0; i < 10; ++i) {
            SystemAdminDO record = map.get((long) i);
            Assert.assertEquals((long) i, record.getId().longValue());
        }

    }
}