const std = @import("std");
const fmt = std.fmt;
const testing = std.testing;
const warn = std.debug.warn;
const meta = std.meta;

const argvT = [][*:0]const u8;

/// Options for our parsing function.
const ParseOptions = struct {
    // Delimits each option name.
    prefix: []const u8 = "-",

    // Are option names case sensitive?
    caseSensitive: bool = false,

    // The input that we will be parsing.
    argv: argvT,
};

const ParseReadStates = enum {
    option_name,
    array,
    value,
};

/// Like the general purpose parser, except this allows us to parse arrays and have arguments without default values.
/// If a field without a default value is not set, an error will be returned.
/// Arrays of arrays/pointers are owned (expected to be freed by) the caller.
pub fn parseAdvanced(base_allocator: *std.mem.Allocator, comptime T: type, options: ParseOptions) anyerror!T {
    var arena = std.heap.ArenaAllocator.init(base_allocator);
    defer arena.deinit();
    const allocator = &arena.allocator;

    var output: T = undefined;

    // Set all fields to null if they can be and have a default value of either None or Null.
    // This is required because when ensuring all fields are set,
    //  we cannot differentiate between an unset field and a field that has been set to null.
    inline for (meta.fields(T)) |f| {
        if (f.default_value == null and @typeInfo(f.field_type) == .Optional) {
            @field(output, f.name) = null;
        }
        // Set default values where applicable.
        // NOTE: currently does not work properly with default values that are arrays of arrays, or anything deeper.
        // By not work properly, there is currently not an guaranteed way for the caller to determine if a value of
        //  the aforementioned type(s) should be freed or not.
        if (f.default_value) |value| {
            @field(output, f.name) = value;
        }
    }

    var fields_to_set = try requiredFields(allocator, T, options.argv);
    defer fields_to_set.deinit();

    var state = ParseReadStates.option_name;
    var argName: []const u8 = undefined;

    var index: usize = 0;
    while (index < options.argv.len) {
        const arg = options.argv[index];
        switch (state) {
            .array => {
                // It is unclear whether the user wants to consider this arg an array member or arg delimiter.
                // Currently we will assume a valid prefix+field_name is intended to be used as an arg delimiter.
                // If this becomes an issue, maybe we could find a workaround later.
                if (isFieldName(T, std.mem.span(arg)[options.prefix.len..])) {
                    state = .option_name;
                    continue;
                }
                inline for (meta.fields(T)) |f| {
                    if (std.mem.eql(u8, f.name, argName)) {
                        const argT = f.field_type;
                        // This if statement exists solely to bypass compilation errors that are not possible in real code.
                        if (@typeInfo(argT) == .Pointer and @typeInfo(meta.Child(f.field_type)) == .Pointer) {
                            const childT = meta.Child(f.field_type);

                            // append value array
                            const new = try stringToValue(childT, std.mem.span(arg));
                            var base = &@field(output, f.name);
                            try append(allocator, childT, new, base);

                            registerSetField(&fields_to_set, f.name);
                            break;
                        }
                    }
                }
            },
            .option_name => {
                argName = std.mem.span(arg)[options.prefix.len..];
                if (isFieldName(T, argName)) {
                    inline for (meta.fields(T)) |f| {
                        if (std.mem.eql(u8, f.name, argName)) {
                            const argT = f.field_type;
                            switch (@typeInfo(argT)) {
                                .Array, .Pointer, .Vector => {
                                    const childT = meta.Child(argT);
                                    switch (@typeInfo(childT)) {
                                        .Array, .Pointer, .Vector => {
                                            state = .array;
                                            @field(output, f.name) = (try allocator.alloc(childT, 0));
                                        },
                                        else => state = .value,
                                    }
                                },
                                else => state = .value,
                            }
                            break;
                        }
                    }
                }
            },
            .value => {
                inline for (meta.fields(T)) |f| {
                    if (std.mem.eql(u8, f.name, argName)) {
                        const argT = f.field_type;
                        if (@typeInfo(argT) == .Pointer and @typeInfo(meta.Child(f.field_type)) == .Pointer) unreachable;
                        @field(output, f.name) = try stringToValue(argT, std.mem.span(arg));
                        registerSetField(&fields_to_set, f.name);
                        break;
                    }
                }
                state = .option_name;
            },
        }
        index += 1;
    }

    // This should be done before we allocate any memory. UNLESS we have a method to detect which fields are allocated, and which are not.
    if (fields_to_set.items.len > 0) {
        std.debug.print("Failed because the following fields have not been set:\n", .{});
        for (fields_to_set.items) |i| {
            std.debug.print("{s}\n", .{i});
        }
        return error.UnsetArguments;
    }

    // Could make all pointer types dynamic at this point, which would simplify freeing the returned structure as well.
    // Instead we will only copy arrays of arrays, arrays being a superset of types (Array, Pointer, Vector).

    // This block of code allocates and copies slices of slices, not the underlying child content.
    // For everything else, a simple direct copy is done as it is assumed the original 'output' variable has those items allocated on the stack or not via our arena allocator.
    // For instance, with '[][]const u8', we will only call alloc([]const u8).
    // This is untested with any other type that corresponds to .Array, .Pointer, .Vector.
    var new_output: T = undefined;
    inline for (meta.fields(T)) |f| {
        const argT = f.field_type;
        switch (@typeInfo(argT)) {
            .Array, .Pointer, .Vector => {
                const childT = meta.Child(argT);
                switch (@typeInfo(childT)) {
                    .Array, .Pointer, .Vector => {
                        const value = @field(output, f.name);
                        var item = try base_allocator.alloc(childT, value.len);
                        for (value) |i, n| item[n] = i;
                        @field(new_output, f.name) = item;
                    },
                    else => {
                        @field(new_output, f.name) = @field(output, f.name);
                    },
                }
            },
            else => {
                @field(new_output, f.name) = @field(output, f.name);
            },
        }
    }

    return new_output;
}

// work in progress prototype. Does this have potential to work?
// will require the original output struct to copy from, or a different function to go through each field and copy all pointer arrays recursively
fn copyPointers(allocator: *std.mem.Allocator, comptime T: type, out: *T) !void {
    switch (@typeInfo(T)) {
        .Pointer => {
            const childT = meta.child(T);
            var item = try allocator.alloc(childT, value.len);
            out.* = item;
            for (item) |*i| {
                copyPointers(allocator, childT, i);
            }
        },
        else => {},
    }
}

fn fieldType(comptime T: type, name: [*:0]const u8) @TypeOf(meta.fields(T)[0]) {
    inline for (meta.fields(T)) |f| {
        @compileLog(@TypeOf(f));
        // if (std.meta.eql(u8, f.name, std.mem.span(name))) return f.field_type;
        if (std.meta.eql(u8, f.name, std.mem.span(name))) return f;
    }
    std.debug.panic("Field not found!\n");
}

fn isFieldName(comptime T: type, name: []const u8) bool {
    inline for (meta.fields(T)) |f| {
        if (std.mem.eql(u8, f.name, std.mem.span(name))) return true;
    }
    return false;
}

fn registerSetField(strings: *Strings, field_name: []const u8) void {
    for (strings.items) |s, index| {
        if (std.mem.eql(u8, s, field_name)) {
            _ = strings.swapRemove(index);
            break;
        }
    }
}

fn append(allocator: *std.mem.Allocator, comptime T: type, new: T, base: *[]T) !void {
    errdefer allocator.free(base.*);
    var new_base = try allocator.alloc(T, base.len + 1);
    for (base.*) |_, index| {
        new_base[index] = base.*[index];
    }
    new_base[new_base.len - 1] = new;
    allocator.free(base.*);
    base.* = new_base;
}

const Strings = std.ArrayList([]const u8);
/// Returns a list of the fields the caller is required to fill out based off the given parse type.
/// A caller is required to fill out a field if it does not have a default value and is not an optional type.
fn requiredFields(allocator: *std.mem.Allocator, comptime T: type, argv: argvT) !Strings {
    var strings = Strings.init(allocator);
    inline for (@typeInfo(T).Struct.fields) |f| {
        if (f.default_value == null and @typeInfo(f.field_type) != .Optional) {
            try strings.append(f.name);
        }
    }
    return strings;
}

fn ensureAllFieldsSet(comptime T: type, argv: [][]const u8) !void {
    // The default value for a field is null in two scenarios:
    // A) the struct literally has '= null' in it for that field.
    // B) the struct has no default value set for that field.
    // This results in an issue of not knowing if a default value is set or not.
    // To work around this issue, we set all fields with a default value of null and field_type of Optional to the value null when our struct is created.
    // From thereon in, we can safely say that any Optional type field with a default value of null has been set.
    // Any non-Optional field with a default_value of null will require being set by argv.

    // Number of fields that do not have default values.
    var required_fields = blk: {
        var rf: u16 = 0;
        inline for (@typeInfo(T).Struct.fields) |f| {
            if (f.default_value == null and @typeInfo(f.field_type) != .Optional) rf += 1;
        }
        break :blk rf;
    };
    // Here we ensure each required field name has been filled out by our given argv.
    // Note: If a string follows a field name, that string will be parsed as that field's value.
    // A bug exists where two field names follow each other. This can be fixed by allowing at least one str after each opt name, or
    //  we could add all required field names to a list, when we add a value to a field, we could remove that from the list.
    // We ignore the final arg because it cannot be filled out with a value.
    // If it is a required field, this would be result in an invalid state.
    for (options.argv[0 .. options.argv.len - 1]) |arg| {
        if (isFieldName(T, arg)) {
            const f = fieldFromName(T, arg);
            if (f.default_value == null and @typeInfo(f.field_type) != .Optional) required_fields -= 1;
        }
    }

    if (required_fields != 0) {
        return error.OptionsNotSet;
    }
}

/// Frees arrays of arrays, which should be the only thing we allocate.
pub fn freeParsed(allocator: *std.mem.Allocator, instance: anytype) void {
    const T = @TypeOf(instance);
    inline for (std.meta.fields(T)) |f| {
        if (@typeInfo(f.field_type) == .Pointer and @typeInfo(std.meta.Child(f.field_type)) == .Pointer) {
            allocator.free(@field(instance, f.name));
        }
    }
}

// test "error on unfilled type" {}

test "parse arrays" {
    const Example = struct {
        a: []const u8,
        c: i10 = 1,
        d: ?i11,
        b: [][]const u8,
    };

    const alloc = testing.allocator;
    var input: [5][*:0]u8 = undefined;
    input[0] = try alloc.dupeZ(u8, "-a");
    input[1] = try alloc.dupeZ(u8, "abc");
    input[2] = try alloc.dupeZ(u8, "-b");
    input[3] = try alloc.dupeZ(u8, "def");
    input[4] = try alloc.dupeZ(u8, "ghi");
    defer {
        for (input) |i| {
            alloc.free(std.mem.span(i));
        }
    }

    const result = try parseAdvanced(
        alloc,
        Example,
        .{ .argv = input[0..] },
    );
    defer alloc.free(result.b);

    try testing.expect(std.mem.eql(u8, result.a, "abc"));
    try testing.expect(std.mem.eql(u8, result.b[0], "def"));
    try testing.expect(std.mem.eql(u8, result.b[1], "ghi"));
    try testing.expectEqual(result.b.len, 2);
}

/// Parses without an allocator.
/// Cannot parse as many types as the other parsing function.
pub fn parseSimple(comptime T: type, options: ParseOptions) anyerror!T {
    var recentOption: ?[]const u8 = null;
    var output = T{};
    var index: usize = 1;
    while (nextArg(index)) |argument| : (index += 1) {
        if (recentOption) |option| {
            inline for (@typeInfo(T).Struct.fields) |f| {
                if ((std.mem.eql(u8, f.name, option)) or (!options.caseSensitive and strLowEql(f.name, option))) {
                    const fieldType = f.field_type;
                    const optionValue = try stringToValue(f.field_type, argument);
                    @field(output, f.name) = optionValue;
                    break;
                }
            }
            recentOption = null;
        } else {
            const isOption = isPrefixed(argument, options.prefix);
            if (isOption) {
                recentOption = argument[options.prefix.len..];
            }
        }
    }
    return output;
}

fn nextArg(index: usize) ?[:0]const u8 {
    var args = std.process.args();
    {
        var remainingSkips = index;
        while (remainingSkips > 0) : (remainingSkips -= 1) {
            _ = args.nextPosix();
        }
    }
    return args.nextPosix();
}

fn isPrefixed(string: []const u8, prefix: []const u8) bool {
    if (prefix.len >= string.len) {
        return false;
    }
    for (prefix) |char, index| {
        if (!(string[index] == char)) {
            return false;
        }
    }
    return true;
}

test "prefix" {
    const prefixes = [_][]const u8{
        "-", "--", "abcdefghijklmnop", "=", "$", "12345",
    };
    const suffixes = [_][]const u8{
        "testjlkj",
        "09fjsdkfjs",
        "lksja",
        "1",
        "x",
        "a-",
    };

    inline for (prefixes) |d| {
        inline for (suffixes) |sfx| {
            const string = d ++ sfx;
            try testing.expect(isPrefixed(string, d));
        }

        inline for (suffixes) |sfx| {
            try testing.expect(!isPrefixed(sfx, d));
            try testing.expect(!isPrefixed(sfx ++ d, d));
        }
    }
}

fn typeOfField(arg: []const u8, comptime structure: type) ?type {
    inline for (@typeInfo(structure).Struct.fields) |f| {
        if (std.mem.eql(u8, f.name, argument)) {
            return f.field_type;
        }
    }
    return null;
}

fn stringToValue(comptime T: type, string: []const u8) anyerror!T {
    const ti = @typeInfo(T);
    switch (ti) {
        .Int => return try fmt.parseInt(T, string, 10),
        .Float => return try fmt.parseFloat(T, string),
        .Bool => {
            if (string[0] == '0' or strLowEql(string, "false")) return false;
            if (string[0] == '1' or strLowEql(string, "true")) return true;
            return error.InvalidBooleanCharacter;
        },
        .Pointer => {
            return string;
        },
        //.Array => {
        //const arrayType = @typeInfo(T.Child);
        //var arrayIndex: usize = 0;
        //var stringIndex: usize = 0;
        //while (arrayIndex < T.len) : (arrayIndex += 1) {
        //    //
        //}
        //},
        .Optional => |info| {
            if (std.mem.eql(u8, string, "null")) return null;
            if (string.len == 0) return null;
            if (string.len == 1 and string[0] == ' ') return null;
            if (string.len == 1 and string[0] == '?') return null;
            return stringToValue(info.child, string) catch null;
        },
        .Enum => |info| {
            inline for (info.fields) |f| {
                if (strLowEql(f.name, string)) {
                    return @intToEnum(T, f.value);
                }
            }
            return error.InvalidEnumName;
        },
        .Union => |info| {
            inline for (info.fields) |f| {
                const maybe_result = stringToValue(f.field_type, string) catch null;
                if (maybe_result) |result| {
                    return @unionInit(T, f.name, result);
                }
            }
            return error.NoUnionValueFound;
        },
        else => @compileError("Unsupported type: " ++ @typeName(T)),
    }
}

fn STVHelper(comptime T: type) type {
    return struct {
        const Self = @This();
        expected: anytype,
        string: []const u8,

        fn runTest(comptime self: Self) !void {
            switch (@typeInfo(@TypeOf(self.expected))) {
                .ErrorSet => try testing.expectError(self.expected, stringToValue(T, self.string)),
                else => {
                    const result = try stringToValue(T, self.string);
                    try testing.expectEqual(@as(T, self.expected), result);
                },
            }
        }
    };
}
test "string to value" {
    const Enum = enum {
        One,
        Two,
    };

    const Union = union(enum) {
        A: u8,
        B: i4,
        C: i8,
        D: bool,
    };
    const UA = Union{ .A = 10 };
    const UB = Union{ .B = -7 };
    const UC = Union{ .C = -8 };
    const UD = Union{ .D = true };
    const UD2 = Union{ .D = false };

    comptime const tests = .{
        STVHelper([]const u8){ .expected = "foobar", .string = "foobar" },
        STVHelper(u8){ .expected = 100, .string = "100" },
        STVHelper(u8){ .expected = error.Overflow, .string = "1000" },
        STVHelper(u8){ .expected = error.Overflow, .string = "-100" },
        STVHelper(u8){ .expected = error.InvalidCharacter, .string = "10a" },
        STVHelper(i8){ .expected = -100, .string = "-100" },
        STVHelper(i8){ .expected = error.Overflow, .string = "-1000" },
        STVHelper(f16){ .expected = 10.5, .string = "10.5" },
        STVHelper(bool){ .expected = true, .string = "true" },
        STVHelper(bool){ .expected = true, .string = "1" },
        STVHelper(bool){ .expected = false, .string = "false" },
        STVHelper(bool){ .expected = false, .string = "0" },
        STVHelper(bool){ .expected = error.InvalidBooleanCharacter, .string = "a" },
        STVHelper(?bool){ .expected = true, .string = "true" },
        STVHelper(?bool){ .expected = false, .string = "false" },

        STVHelper(?bool){ .expected = null, .string = "" },
        STVHelper(?bool){ .expected = null, .string = " " },
        STVHelper(?bool){ .expected = null, .string = "null" },
        STVHelper(?u8){ .expected = null, .string = "foobar" },
        STVHelper(?u8){ .expected = null, .string = "300" },

        STVHelper(Enum){ .expected = .One, .string = "one" },
        STVHelper(Enum){ .expected = .Two, .string = "TwO" },
        STVHelper(Enum){ .expected = error.InvalidEnumName, .string = "three" },

        STVHelper(Union){ .expected = UA, .string = "10" },
        // Union items are chosen based on order of fields. First valid field is chosen
        STVHelper(Union){ .expected = UB, .string = "-7" },
        STVHelper(Union){ .expected = UC, .string = "-8" },
        STVHelper(Union){ .expected = UD, .string = "true" },
        STVHelper(Union){ .expected = UD2, .string = "false" },
        STVHelper(Union){ .expected = error.NoUnionValueFound, .string = "a" },
        STVHelper(Union){ .expected = error.NoUnionValueFound, .string = "256" },
    };

    inline for (tests) |t| {
        try t.runTest();
    }
}

inline fn lowercase(char: u8) u8 {
    if (char >= 'A' and char <= 'Z') {
        const charr: i9 = char;
        const zz: i9 = 'Z' - 'z';
        return @intCast(u8, charr - zz);
    }
    return char;
}

fn strLowEql(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a) |i, index| {
        if (lowercase(i) != lowercase(b[index])) return false;
    }
    return true;
}
