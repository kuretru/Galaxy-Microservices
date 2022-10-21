package com.kuretru.microservices.authentication.util;

import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
class RoleUtilsTest {

    private final Set<String> expected;

    RoleUtilsTest() {
        expected = new HashSet<>(16);
        expected.add("a");
        expected.add("b");
        expected.add("c");
    }

    @Test
    void hasRole() {
        assertTrue(RoleUtils.hasRole(expected, "a"));
        assertTrue(RoleUtils.hasRole(expected, "b"));
        assertTrue(RoleUtils.hasRole(expected, "c"));
        assertFalse(RoleUtils.hasRole(expected, "d"));

        assertTrue(RoleUtils.hasRole(expected, new String[]{"a"}));
        assertTrue(RoleUtils.hasRole(expected, new String[]{"a", "b"}));
        assertTrue(RoleUtils.hasRole(expected, new String[]{"a", "d"}));
        assertFalse(RoleUtils.hasRole(expected, new String[]{"d", "e"}));
    }

    @Test
    void hasRoles() {
        assertTrue(RoleUtils.hasRoles(expected, "a"));
        assertTrue(RoleUtils.hasRoles(expected, "b"));
        assertTrue(RoleUtils.hasRoles(expected, "c"));
        assertFalse(RoleUtils.hasRoles(expected, "d"));

        assertTrue(RoleUtils.hasRoles(expected, new String[]{"a", "b", "c"}));
        assertTrue(RoleUtils.hasRoles(expected, new String[]{"a", "b", "c", "d"}));
        assertFalse(RoleUtils.hasRoles(expected, new String[]{"a", "b"}));
        assertFalse(RoleUtils.hasRoles(expected, new String[]{"a", "d"}));
        assertFalse(RoleUtils.hasRoles(expected, new String[]{"d", "e"}));
    }

}
