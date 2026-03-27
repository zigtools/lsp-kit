//! Conversion functions between the following Units:
//! - A "index" or "source index" is a offset into a utf-8 encoding source file.
//! - `Loc`
//! - `Position`
//! - `Range`

const std = @import("std");
const types = @import("types");

/// Specifies how the `character` field in `Position` is defined.
/// The Character encoding is negotiated during initialization with the Client/Editor.
pub const Encoding = enum {
    /// Character offsets count UTF-8 code units (e.g. bytes).
    @"utf-8",
    /// Character offsets count UTF-16 code units.
    ///
    /// This is the default and must always be supported
    /// by servers
    @"utf-16",
    /// Character offsets count UTF-32 code units.
    ///
    /// Implementation note: these are the same as Unicode codepoints,
    /// so this `PositionEncodingKind` may also be used for an
    /// encoding-agnostic representation of character offsets.
    @"utf-32",
};

/// A pair of two source indexes into a document.
/// Asserts that `start <= end`.
pub const Loc = std.zig.Token.Loc;
pub const Position = types.Position;
pub const Range = types.Range;

pub fn indexToPosition(text: []const u8, index: usize, encoding: Encoding) Position {
    const last_line_start = if (std.mem.findScalarLast(u8, text[0..index], '\n')) |line| line + 1 else 0;
    const line_count = std.mem.count(u8, text[0..last_line_start], "\n");

    return .{
        .line = @intCast(line_count),
        .character = @intCast(countCodeUnits(text[last_line_start..index], encoding)),
    };
}

pub fn positionToIndex(text: []const u8, position: Position, encoding: Encoding) usize {
    var line: u32 = 0;
    var line_start_index: usize = 0;
    for (text, 0..) |c, i| {
        if (line == position.line) break;
        if (c == '\n') {
            line += 1;
            line_start_index = i + 1;
        }
    } else return text.len;

    const line_text = std.mem.sliceTo(text[line_start_index..], '\n');
    const line_byte_length = getNCodeUnitByteCount(line_text, position.character, encoding);

    return line_start_index + line_byte_length;
}

test "index <-> Position" {
    try testIndexPosition("", 0, 0, .{ 0, 0, 0 });

    try testIndexPosition("hello from zig", 10, 0, .{ 10, 10, 10 });

    try testIndexPosition("\n", 0, 0, .{ 0, 0, 0 });
    try testIndexPosition("\n", 1, 1, .{ 0, 0, 0 });

    try testIndexPosition("hello\nfrom\nzig\n", 5, 0, .{ 5, 5, 5 });
    try testIndexPosition("hello\nfrom\nzig\n", 6, 1, .{ 0, 0, 0 });
    try testIndexPosition("hello\nfrom\nzig\n", 8, 1, .{ 2, 2, 2 });
    try testIndexPosition("\nhello\nfrom\nzig", 15, 3, .{ 3, 3, 3 });

    try testIndexPosition("a¶↉🠁", 10, 0, .{ 10, 5, 4 });
    try testIndexPosition("🇺🇸 🇩🇪", 17, 0, .{ 17, 9, 5 });

    try testIndexPosition("a¶↉🠁\na¶↉🠁", 10, 0, .{ 10, 5, 4 });
    try testIndexPosition("a¶↉🠁\na¶↉🠁", 11, 1, .{ 0, 0, 0 });
    try testIndexPosition("a¶↉🠁\na¶↉🠁", 21, 1, .{ 10, 5, 4 });

    try testIndexPosition("\na¶↉🠁", 4, 1, .{ 3, 2, 2 });
    try testIndexPosition("a¶↉🠁\n", 6, 0, .{ 6, 3, 3 });
    try testIndexPosition("a¶↉🠁\n", 11, 1, .{ 0, 0, 0 });
}

fn testIndexPosition(text: []const u8, index: usize, line: u32, characters: [3]u32) !void {
    const position8: Position = .{ .line = line, .character = characters[0] };
    const position16: Position = .{ .line = line, .character = characters[1] };
    const position32: Position = .{ .line = line, .character = characters[2] };

    try std.testing.expectEqual(position8, indexToPosition(text, index, .@"utf-8"));
    try std.testing.expectEqual(position16, indexToPosition(text, index, .@"utf-16"));
    try std.testing.expectEqual(position32, indexToPosition(text, index, .@"utf-32"));

    try std.testing.expectEqual(index, positionToIndex(text, position8, .@"utf-8"));
    try std.testing.expectEqual(index, positionToIndex(text, position16, .@"utf-16"));
    try std.testing.expectEqual(index, positionToIndex(text, position32, .@"utf-32"));
}

test "positionToIndex where character value is greater than the line length" {
    try testPositionToIndex("", 0, 0, .{ 1, 1, 1 });

    try testPositionToIndex("\n", 0, 0, .{ 1, 1, 1 });
    try testPositionToIndex("\n", 0, 0, .{ 2, 2, 2 });
    try testPositionToIndex("\n", 0, 0, .{ 3, 3, 3 });

    try testPositionToIndex("\n", 1, 1, .{ 1, 1, 1 });
    try testPositionToIndex("\n", 1, 1, .{ 2, 2, 2 });
    try testPositionToIndex("\n", 1, 1, .{ 3, 3, 3 });

    try testPositionToIndex("hello\nfrom\nzig\n", 5, 0, .{ 6, 6, 6 });
    try testPositionToIndex("hello\nfrom\nzig\n", 10, 1, .{ 5, 5, 5 });

    try testPositionToIndex("a¶↉🠁\na¶↉🠁", 21, 1, .{ 11, 6, 5 });
    try testPositionToIndex("a¶↉🠁\na¶↉🠁\n", 21, 1, .{ 11, 6, 5 });
}

test "positionToIndex where line value is greater than the number of lines" {
    try testPositionToIndex("", 0, 1, .{ 0, 0, 0 });
    try testPositionToIndex("", 0, 1, .{ 3, 2, 1 });

    try testPositionToIndex("hello", 5, 1, .{ 0, 0, 0 });
    try testPositionToIndex("hello", 5, 1, .{ 3, 2, 1 });

    try testPositionToIndex("hello\nfrom\nzig", 14, 3, .{ 0, 0, 0 });
    try testPositionToIndex("hello\nfrom\nzig", 14, 3, .{ 3, 2, 1 });
}

fn testPositionToIndex(text: []const u8, index: usize, line: u32, characters: [3]u32) !void {
    const position8: Position = .{ .line = line, .character = characters[0] };
    const position16: Position = .{ .line = line, .character = characters[1] };
    const position32: Position = .{ .line = line, .character = characters[2] };

    try std.testing.expectEqual(index, positionToIndex(text, position8, .@"utf-8"));
    try std.testing.expectEqual(index, positionToIndex(text, position16, .@"utf-16"));
    try std.testing.expectEqual(index, positionToIndex(text, position32, .@"utf-32"));
}

pub fn locLength(text: []const u8, loc: Loc, encoding: Encoding) usize {
    return countCodeUnits(text[loc.start..loc.end], encoding);
}

test locLength {
    const text = "a¶↉🠁";
    try std.testing.expectEqual(10, locLength(text, .{ .start = 0, .end = text.len }, .@"utf-8"));
    try std.testing.expectEqual(5, locLength(text, .{ .start = 0, .end = text.len }, .@"utf-16"));
    try std.testing.expectEqual(4, locLength(text, .{ .start = 0, .end = text.len }, .@"utf-32"));
}

pub fn rangeLength(text: []const u8, range: Range, encoding: Encoding) usize {
    const loc = rangeToLoc(text, range, encoding);
    return locLength(text, loc, encoding);
}

test rangeLength {
    const text = "a¶↉🠁";
    try std.testing.expectEqual(10, rangeLength(text, .{ .start = .{ .line = 0, .character = 0 }, .end = .{ .line = 0, .character = 10 } }, .@"utf-8"));
    try std.testing.expectEqual(5, rangeLength(text, .{ .start = .{ .line = 0, .character = 0 }, .end = .{ .line = 0, .character = 5 } }, .@"utf-16"));
    try std.testing.expectEqual(4, rangeLength(text, .{ .start = .{ .line = 0, .character = 0 }, .end = .{ .line = 0, .character = 4 } }, .@"utf-32"));
}

pub fn locToSlice(text: []const u8, loc: Loc) []const u8 {
    return text[loc.start..loc.end];
}

test locToSlice {
    try std.testing.expectEqualStrings("per", locToSlice("Superstition", .{ .start = 2, .end = 5 }));
}

pub fn locToRange(text: []const u8, loc: Loc, encoding: Encoding) Range {
    std.debug.assert(loc.start <= loc.end and loc.end <= text.len);
    const start = indexToPosition(text, loc.start, encoding);
    return .{
        .start = start,
        .end = advancePosition(text, start, loc.start, loc.end, encoding),
    };
}

test locToRange {
    const utf8_range: Range = .{
        .start = .{ .line = 0, .character = 4 }, // '𐀀'
        .end = .{ .line = 0, .character = 4 + 1 + 2 }, // '𐀀' + 'a' + '¶'
    };

    const utf16_range: Range = .{
        .start = .{ .line = 0, .character = 2 }, // '𐀀'
        .end = .{ .line = 0, .character = 2 + 1 + 1 }, // '𐀀' + 'a' + '¶'
    };

    const utf32_range: Range = .{
        .start = .{ .line = 0, .character = 1 }, // '𐀀'
        .end = .{ .line = 0, .character = 1 + 1 + 1 }, // '𐀀' + 'a' + '¶'
    };

    try std.testing.expectEqualDeep(utf8_range, locToRange("𐀀a¶↉", .{ .start = 4, .end = 7 }, .@"utf-8"));
    try std.testing.expectEqualDeep(utf16_range, locToRange("𐀀a¶↉", .{ .start = 4, .end = 7 }, .@"utf-16"));
    try std.testing.expectEqualDeep(utf32_range, locToRange("𐀀a¶↉", .{ .start = 4, .end = 7 }, .@"utf-32"));
}

pub fn rangeToSlice(text: []const u8, range: Range, encoding: Encoding) []const u8 {
    return locToSlice(text, rangeToLoc(text, range, encoding));
}

pub fn rangeToLoc(text: []const u8, range: Range, encoding: Encoding) Loc {
    std.debug.assert(orderPosition(range.start, range.end) != .gt);
    const start = positionToIndex(text, range.start, encoding);

    const end_position_relative_to_start: Position = .{
        .line = range.end.line - range.start.line,
        .character = if (range.start.line == range.end.line)
            range.end.character - range.start.character
        else
            range.end.character,
    };

    const relative_end = positionToIndex(text[start..], end_position_relative_to_start, encoding);
    return .{ .start = start, .end = start + relative_end };
}

test rangeToSlice {
    const expect = std.testing.expectEqualStrings;

    try expect("", rangeToSlice(
        "",
        .{
            .start = .{ .line = 0, .character = 0 },
            .end = .{ .line = 0, .character = 0 },
        },
        .@"utf-8",
    ));
    try expect("-A-", rangeToSlice(
        "Peek-A-Boo",
        .{
            .start = .{ .line = 0, .character = 4 },
            .end = .{ .line = 0, .character = 7 },
        },
        .@"utf-8",
    ));
    try expect("ek\nA\nB", rangeToSlice(
        "Peek\nA\nBoo",
        .{
            .start = .{ .line = 0, .character = 2 },
            .end = .{ .line = 2, .character = 1 },
        },
        .@"utf-8",
    ));
}

pub fn lineLocAtIndex(text: []const u8, index: usize) Loc {
    return .{
        .start = if (std.mem.findScalarLast(u8, text[0..index], '\n')) |idx| idx + 1 else 0,
        .end = std.mem.findScalarPos(u8, text, index, '\n') orelse text.len,
    };
}

test lineLocAtIndex {
    const expect = std.testing.expectEqual;

    try expect(Loc{ .start = 0, .end = 0 }, lineLocAtIndex("", 0));
    try expect(Loc{ .start = 0, .end = 0 }, lineLocAtIndex("\n", 0));
    try expect(Loc{ .start = 1, .end = 1 }, lineLocAtIndex("\n", 1));

    try expect(Loc{ .start = 0, .end = 3 }, lineLocAtIndex("foo\n", 3));
    try expect(Loc{ .start = 4, .end = 4 }, lineLocAtIndex("foo\n", 4));

    try expect(Loc{ .start = 0, .end = 3 }, lineLocAtIndex("foo\nbar", 0));
    try expect(Loc{ .start = 0, .end = 3 }, lineLocAtIndex("foo\nbar", 3));
    try expect(Loc{ .start = 4, .end = 7 }, lineLocAtIndex("foo\nbar", 4));
    try expect(Loc{ .start = 4, .end = 7 }, lineLocAtIndex("foo\nbar", 5));
    try expect(Loc{ .start = 4, .end = 7 }, lineLocAtIndex("foo\nbar", 7));
}

pub fn lineSliceAtIndex(text: []const u8, index: usize) []const u8 {
    return locToSlice(text, lineLocAtIndex(text, index));
}

test lineSliceAtIndex {
    try std.testing.expectEqualStrings("Sealed", lineSliceAtIndex("Signed\nSealed\nDelivered", 10));
}

pub fn lineLocAtPosition(text: []const u8, position: Position, encoding: Encoding) Loc {
    return lineLocAtIndex(text, positionToIndex(text, position, encoding));
}

test lineLocAtPosition {
    try std.testing.expectEqual(Loc{ .start = 7, .end = 10 }, lineLocAtPosition("Living\nFor\nThe\nCity", .{ .line = 1, .character = 2 }, .@"utf-8"));
}

pub fn lineSliceAtPosition(text: []const u8, position: Position, encoding: Encoding) []const u8 {
    return locToSlice(text, lineLocAtPosition(text, position, encoding));
}

test lineSliceAtPosition {
    try std.testing.expectEqualStrings("The", lineSliceAtPosition("Ribbon\nIn\nThe\nSky", .{ .line = 2, .character = 3 }, .@"utf-8"));
}

pub fn lineLocUntilIndex(text: []const u8, index: usize) Loc {
    return .{
        .start = if (std.mem.findScalarLast(u8, text[0..index], '\n')) |idx| idx + 1 else 0,
        .end = index,
    };
}

test lineLocUntilIndex {
    const expect = std.testing.expectEqual;

    try expect(Loc{ .start = 0, .end = 0 }, lineLocUntilIndex("", 0));

    try expect(Loc{ .start = 0, .end = 0 }, lineLocUntilIndex("hello", 0));
    try expect(Loc{ .start = 0, .end = 3 }, lineLocUntilIndex("hello", 3));
    try expect(Loc{ .start = 0, .end = 5 }, lineLocUntilIndex("hello", 5));

    try expect(Loc{ .start = 0, .end = 5 }, lineLocUntilIndex("hello\nworld", 5));
    try expect(Loc{ .start = 6, .end = 6 }, lineLocUntilIndex("hello\nworld", 6));
    try expect(Loc{ .start = 6, .end = 11 }, lineLocUntilIndex("hello\nworld", 11));
}

pub fn lineSliceUntilIndex(text: []const u8, index: usize) []const u8 {
    return locToSlice(text, lineLocUntilIndex(text, index));
}

test lineSliceUntilIndex {
    try std.testing.expectEqualStrings("O", lineSliceUntilIndex("One\nOf\nA\nKind", 5));
}

pub fn lineLocUntilPosition(text: []const u8, position: Position, encoding: Encoding) Loc {
    return lineLocUntilIndex(text, positionToIndex(text, position, encoding));
}

test lineLocUntilPosition {
    try std.testing.expectEqual(Loc{ .start = 4, .end = 8 }, lineLocUntilPosition("You\nHavent\nDone\nNothin", .{ .line = 1, .character = 4 }, .@"utf-8"));
}

pub fn lineSliceUntilPosition(text: []const u8, position: Position, encoding: Encoding) []const u8 {
    return locToSlice(text, lineLocUntilPosition(text, position, encoding));
}

test lineSliceUntilPosition {
    try std.testing.expectEqualStrings("Dr", lineSliceUntilPosition("Dont\nDrive\nDrunk", .{ .line = 1, .character = 2 }, .@"utf-8"));
}

pub fn convertPositionEncoding(text: []const u8, position: Position, from_encoding: Encoding, to_encoding: Encoding) Position {
    if (from_encoding == to_encoding) return position;

    const line_loc = lineLocUntilPosition(text, position, from_encoding);

    return .{
        .line = position.line,
        .character = @intCast(locLength(text, line_loc, to_encoding)),
    };
}

test convertPositionEncoding {
    try testConvertPositionEncoding("", .{ .line = 0, .character = 0 }, .{ .utf8 = 0, .utf16 = 0, .utf32 = 0 });
    try testConvertPositionEncoding("\n", .{ .line = 0, .character = 0 }, .{ .utf8 = 0, .utf16 = 0, .utf32 = 0 });
    try testConvertPositionEncoding("\n", .{ .line = 1, .character = 0 }, .{ .utf8 = 0, .utf16 = 0, .utf32 = 0 });
    try testConvertPositionEncoding("foo", .{ .line = 0, .character = 3 }, .{ .utf8 = 3, .utf16 = 3, .utf32 = 3 });
    try testConvertPositionEncoding("a¶↉🠁", .{ .line = 0, .character = 10 }, .{ .utf8 = 10, .utf16 = 5, .utf32 = 4 });
    try testConvertPositionEncoding("a¶↉🠁\na¶↉🠁", .{ .line = 1, .character = 6 }, .{ .utf8 = 6, .utf16 = 3, .utf32 = 3 });
}

fn testConvertPositionEncoding(text: [:0]const u8, position: Position, new_characters: struct { utf8: u32, utf16: u32, utf32: u32 }) !void {
    const position8 = convertPositionEncoding(text, position, .@"utf-8", .@"utf-8");
    const position16 = convertPositionEncoding(text, position, .@"utf-8", .@"utf-16");
    const position32 = convertPositionEncoding(text, position, .@"utf-8", .@"utf-32");

    try std.testing.expectEqual(position.line, position8.line);
    try std.testing.expectEqual(position.line, position16.line);
    try std.testing.expectEqual(position.line, position32.line);

    try std.testing.expectEqual(new_characters.utf8, position8.character);
    try std.testing.expectEqual(new_characters.utf16, position16.character);
    try std.testing.expectEqual(new_characters.utf32, position32.character);
}

pub fn convertRangeEncoding(text: []const u8, range: Range, from_encoding: Encoding, to_encoding: Encoding) Range {
    std.debug.assert(orderPosition(range.start, range.end) != .gt);
    if (from_encoding == to_encoding) return range;
    return .{
        .start = convertPositionEncoding(text, range.start, from_encoding, to_encoding),
        .end = convertPositionEncoding(text, range.end, from_encoding, to_encoding),
    };
}

test convertRangeEncoding {
    const utf8_range: Range = .{
        .start = .{ .line = 0, .character = 1 + 2 }, // 'a' + '¶'
        .end = .{ .line = 0, .character = 1 + 2 + 3 + 4 }, // 'a' + '¶' + '↉' + '🠁'
    };

    const utf16_range: Range = .{
        .start = .{ .line = 0, .character = 1 + 1 }, // 'a' + '¶'
        .end = .{ .line = 0, .character = 1 + 1 + 1 + 2 }, // 'a' + '¶' + '↉' + '🠁'
    };

    try std.testing.expectEqualDeep(utf8_range, convertRangeEncoding("a¶↉🠁", utf16_range, .@"utf-16", .@"utf-8"));
    try std.testing.expectEqualDeep(utf16_range, convertRangeEncoding("a¶↉🠁", utf8_range, .@"utf-8", .@"utf-16"));
}

pub fn orderPosition(a: Position, b: Position) std.math.Order {
    const line_order = std.math.order(a.line, b.line);
    if (line_order != .eq) return line_order;
    return std.math.order(a.character, b.character);
}

test orderPosition {
    const expect = std.testing.expectEqual;

    try expect(.lt, orderPosition(.{ .line = 1, .character = 0 }, .{ .line = 3, .character = 5 }));
    try expect(.lt, orderPosition(.{ .line = 1, .character = 3 }, .{ .line = 3, .character = 5 }));
    try expect(.lt, orderPosition(.{ .line = 1, .character = 6 }, .{ .line = 3, .character = 5 }));
    try expect(.lt, orderPosition(.{ .line = 3, .character = 0 }, .{ .line = 3, .character = 5 }));

    try expect(.eq, orderPosition(.{ .line = 3, .character = 3 }, .{ .line = 3, .character = 3 }));

    try expect(.gt, orderPosition(.{ .line = 3, .character = 6 }, .{ .line = 3, .character = 3 }));
    try expect(.gt, orderPosition(.{ .line = 5, .character = 0 }, .{ .line = 3, .character = 5 }));
    try expect(.gt, orderPosition(.{ .line = 5, .character = 3 }, .{ .line = 3, .character = 5 }));
    try expect(.gt, orderPosition(.{ .line = 5, .character = 6 }, .{ .line = 3, .character = 5 }));
}

/// Advance `position` which starts at `from_index` to `to_index` accounting for line breaks.
/// The return value will always be `indexToPosition(text, to_index, encoding)`.
pub fn advancePosition(text: []const u8, position: Position, from_index: usize, to_index: usize, encoding: Encoding) Position {
    var line = position.line;

    for (text[from_index..to_index]) |c| {
        if (c == '\n') {
            line += 1;
        }
    }

    const line_loc = lineLocUntilIndex(text, to_index);

    return .{
        .line = line,
        .character = @intCast(locLength(text, line_loc, encoding)),
    };
}

test advancePosition {
    try testAdvancePosition("", .{ .line = 0, .character = 0 }, 0, 0);
    try testAdvancePosition("foo", .{ .line = 0, .character = 0 }, 0, 3);
    try testAdvancePosition("\n", .{ .line = 0, .character = 0 }, 0, 1);
    try testAdvancePosition("foo\nbar", .{ .line = 0, .character = 1 }, 1, 6);
    try testAdvancePosition("foo\nbar", .{ .line = 1, .character = 0 }, 4, 7);
}

fn testAdvancePosition(text: []const u8, position: Position, from: usize, to: usize) !void {
    try std.testing.expectEqual(indexToPosition(text, to, .@"utf-16"), advancePosition(text, position, from, to, .@"utf-16"));
}

/// returns the number of code units in `text`
pub fn countCodeUnits(text: []const u8, encoding: Encoding) usize {
    switch (encoding) {
        .@"utf-8" => return text.len,
        .@"utf-16" => {
            var iter: std.unicode.Utf8Iterator = .{ .bytes = text, .i = 0 };

            var utf16_len: usize = 0;
            while (iter.nextCodepoint()) |codepoint| {
                if (codepoint < 0x10000) {
                    utf16_len += 1;
                } else {
                    utf16_len += 2;
                }
            }
            return utf16_len;
        },
        .@"utf-32" => return std.unicode.utf8CountCodepoints(text) catch unreachable,
    }
}

test countCodeUnits {
    try testCountCodeUnits("", .{ .utf8 = 0, .utf16 = 0, .utf32 = 0 });
    try testCountCodeUnits("a\na", .{ .utf8 = 3, .utf16 = 3, .utf32 = 3 });
    try testCountCodeUnits("a¶↉🠁", .{ .utf8 = 10, .utf16 = 5, .utf32 = 4 });
    try testCountCodeUnits("🠁↉¶a", .{ .utf8 = 10, .utf16 = 5, .utf32 = 4 });
    try testCountCodeUnits("🇺🇸 🇩🇪", .{ .utf8 = 17, .utf16 = 9, .utf32 = 5 });
}

fn testCountCodeUnits(text: []const u8, expected: struct { utf8: usize, utf16: usize, utf32: usize }) !void {
    try std.testing.expectEqual(expected.utf8, countCodeUnits(text, .@"utf-8"));
    try std.testing.expectEqual(expected.utf16, countCodeUnits(text, .@"utf-16"));
    try std.testing.expectEqual(expected.utf32, countCodeUnits(text, .@"utf-32"));
}

/// Returns the number of (utf-8 code units / bytes) that represent `n` code units in `text`.
/// If `text` has less than `n` code units then the number of code units in
/// `text` are returned, i.e. the result is being clamped.
pub fn getNCodeUnitByteCount(text: []const u8, n: usize, encoding: Encoding) usize {
    switch (encoding) {
        .@"utf-8" => return @min(text.len, n),
        .@"utf-16" => {
            if (n == 0) return 0;
            var iter: std.unicode.Utf8Iterator = .{ .bytes = text, .i = 0 };

            var utf16_len: usize = 0;
            while (iter.nextCodepoint()) |codepoint| {
                if (codepoint < 0x10000) {
                    utf16_len += 1;
                } else {
                    utf16_len += 2;
                }
                if (utf16_len >= n) break;
            }
            return iter.i;
        },
        .@"utf-32" => {
            var i: usize = 0;
            var count: usize = 0;
            while (count != n) : (count += 1) {
                if (i >= text.len) break;
                i += std.unicode.utf8ByteSequenceLength(text[i]) catch unreachable;
            }
            return i;
        },
    }
}

test getNCodeUnitByteCount {
    try testGetNCodeUnitByteCount("", .{ .utf8 = 0, .utf16 = 0, .utf32 = 0 });
    try testGetNCodeUnitByteCount("foo", .{ .utf8 = 2, .utf16 = 2, .utf32 = 2 });
    try testGetNCodeUnitByteCount("a¶🠁🠁", .{ .utf8 = 7, .utf16 = 4, .utf32 = 3 });
    try testGetNCodeUnitByteCount("🇺🇸 🇩🇪", .{ .utf8 = 9, .utf16 = 5, .utf32 = 3 });
}

fn testGetNCodeUnitByteCount(text: []const u8, expected: struct { utf8: usize, utf16: usize, utf32: usize }) !void {
    try std.testing.expectEqual(expected.utf8, getNCodeUnitByteCount(text, expected.utf8, .@"utf-8"));
    try std.testing.expectEqual(expected.utf8, getNCodeUnitByteCount(text, expected.utf16, .@"utf-16"));
    try std.testing.expectEqual(expected.utf8, getNCodeUnitByteCount(text, expected.utf32, .@"utf-32"));
}
