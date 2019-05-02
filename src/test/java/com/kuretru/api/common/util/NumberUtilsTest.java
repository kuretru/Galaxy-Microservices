package com.kuretru.api.common.util;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * @author 呉真 Kuretru < kuretru@gmail.com >
 */
public class NumberUtilsTest {

    @Test
    public void toPercentage() {
        double value = 0.987654321;
        double percentage = NumberUtils.toPercentage(value);
        assertEquals(new Double(98.77), new Double(percentage));
    }

}