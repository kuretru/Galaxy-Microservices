package com.kuretru.api.common.util;

import org.junit.jupiter.api.Test;

import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
class InstantUtilsTest {

    private static final String TIME = "2000-02-29 12:34:56";
    private static final String TIME_NEW_YORK = "20000229 123456";
    private static final int YEAR = 2000;
    private static final int MONTH = 2;
    private static final int DAY = 29;
    private static final int HOUR = 12;
    private static final int MINUTE = 34;
    private static final int SECOND = 56;
    private static final String DATE_FORMAT = "yyyyMMdd HHmmss";
    private static final ZoneId ZONE_ID = ZoneId.of("America/New_York");
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern(DATE_FORMAT).withZone(ZONE_ID);

    @Test
    void testToString() {
        ZonedDateTime zonedDateTime = ZonedDateTime.of(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND,
                0, InstantUtils.DEFAULT_ZONE_ID);
        Instant instant = zonedDateTime.toInstant();
        String time = InstantUtils.toString(instant);
        assertEquals(TIME, time);

        zonedDateTime = ZonedDateTime.of(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, 0, ZONE_ID);
        instant = zonedDateTime.toInstant();
        time = InstantUtils.toString(instant, FORMATTER);
        assertEquals(TIME_NEW_YORK, time);
    }

    @Test
    void fromString() {
        Instant instant = InstantUtils.fromString(TIME);
        ZonedDateTime zonedDateTime = ZonedDateTime.ofInstant(instant, InstantUtils.DEFAULT_ZONE_ID);
        assertEquals(YEAR, zonedDateTime.getYear());
        assertEquals(MONTH, zonedDateTime.getMonthValue());
        assertEquals(DAY, zonedDateTime.getDayOfMonth());
        assertEquals(HOUR, zonedDateTime.getHour());
        assertEquals(MINUTE, zonedDateTime.getMinute());
        assertEquals(SECOND, zonedDateTime.getSecond());

        instant = InstantUtils.fromString(TIME_NEW_YORK, FORMATTER);
        zonedDateTime = ZonedDateTime.ofInstant(instant, ZONE_ID);
        assertEquals(YEAR, zonedDateTime.getYear());
        assertEquals(MONTH, zonedDateTime.getMonthValue());
        assertEquals(DAY, zonedDateTime.getDayOfMonth());
        assertEquals(HOUR, zonedDateTime.getHour());
        assertEquals(MINUTE, zonedDateTime.getMinute());
        assertEquals(SECOND, zonedDateTime.getSecond());
    }

}