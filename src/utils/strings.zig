const std = @import("std");

pub fn trim(input: []const u8) []const u8 {
    return std.mem.trim(u8, input, " \t\n\r");
}

/// Caller owns returned memory
pub fn copy(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    return allocator.dupe(u8, source);
}

/// Extract JIRA project prefix from branch name like "feature/PDI-123-description"
/// Returns null if pattern doesn't match
pub fn extractJiraPrefix(branch_name: []const u8) ?[]const u8 {
    // Skip "feature/" prefix if present
    var name = branch_name;
    if (std.mem.startsWith(u8, name, "feature/")) {
        name = name[8..];
    } else if (std.mem.startsWith(u8, name, "bugfix/")) {
        name = name[7..];
    }

    // Look for pattern like "ABC-123"
    var i: usize = 0;
    while (i < name.len and std.ascii.isAlphabetic(name[i])) : (i += 1) {}

    if (i == 0 or i >= name.len or name[i] != '-') {
        return null;
    }

    // Verify there's a number after the dash
    var j = i + 1;
    while (j < name.len and std.ascii.isDigit(name[j])) : (j += 1) {}

    if (j == i + 1) {
        return null; // No digits found
    }

    return name[0..i]; // Return just the project prefix (e.g., "PDI")
}

/// Format current timestamp as ISO 8601
pub fn formatTimestamp(allocator: std.mem.Allocator) ![]const u8 {
    const timestamp = std.time.timestamp();
    const epoch_seconds: u64 = @intCast(timestamp);
    const epoch = std.time.epoch.EpochSeconds{ .secs = epoch_seconds };
    const day = epoch.getEpochDay();
    const year_day = day.calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    const day_seconds = epoch.getDaySeconds();

    return std.fmt.allocPrint(allocator, "{d:0>4}-{d:0>2}-{d:0>2}T{d:0>2}:{d:0>2}:{d:0>2}Z", .{
        year_day.year,
        month_day.month.numeric(),
        month_day.day_index + 1,
        day_seconds.getHoursIntoDay(),
        day_seconds.getMinutesIntoHour(),
        day_seconds.getSecondsIntoMinute(),
    });
}

/// Escape a string for YAML (basic escaping)
pub fn escapeYaml(allocator: std.mem.Allocator, input: []const u8) ![]const u8 {
    var result: std.ArrayListUnmanaged(u8) = .{};
    errdefer result.deinit(allocator);

    for (input) |c| {
        switch (c) {
            '\\' => try result.appendSlice(allocator, "\\\\"),
            '"' => try result.appendSlice(allocator, "\\\""),
            '\n' => try result.appendSlice(allocator, "\\n"),
            '\r' => try result.appendSlice(allocator, "\\r"),
            '\t' => try result.appendSlice(allocator, "\\t"),
            else => try result.append(allocator, c),
        }
    }

    return result.toOwnedSlice(allocator);
}

/// Check if string needs YAML quoting
pub fn needsYamlQuoting(input: []const u8) bool {
    if (input.len == 0) return true;

    // Check first character for special meaning
    const first = input[0];
    if (first == '-' or first == ':' or first == '#' or first == '&' or
        first == '*' or first == '!' or first == '|' or first == '>' or
        first == '\'' or first == '"' or first == '%' or first == '@' or
        first == '`' or first == '{' or first == '[')
    {
        return true;
    }

    // Check for colons followed by space, or special chars
    for (input, 0..) |c, i| {
        if (c == ':' and i + 1 < input.len and input[i + 1] == ' ') return true;
        if (c == '#') return true;
        if (c == '\n' or c == '\r') return true;
    }

    return false;
}

test "extractJiraPrefix" {
    const prefix1 = extractJiraPrefix("feature/PDI-123-some-description");
    try std.testing.expectEqualStrings("PDI", prefix1.?);

    const prefix2 = extractJiraPrefix("feature/ABC-1-test");
    try std.testing.expectEqualStrings("ABC", prefix2.?);

    const prefix3 = extractJiraPrefix("main");
    try std.testing.expect(prefix3 == null);

    const prefix4 = extractJiraPrefix("develop");
    try std.testing.expect(prefix4 == null);
}
