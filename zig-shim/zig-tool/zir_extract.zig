/// zir-extract: Parse a Zig source file and emit ZIR as OrganIR JSON.
///
/// Uses std.zig.Ast.parse() and std.zig.AstGen.generate() from the Zig
/// standard library to produce ZIR (Zig Intermediate Representation).
/// ZIR is *untyped* — it is pre-semantic-analysis IR produced by AstGen.
///
/// Output: OrganIR JSON on stdout.
/// Errors: diagnostic text on stderr.
const std = @import("std");
const Ast = std.zig.Ast;
const AstGen = std.zig.AstGen;
const Zir = std.zig.Zir;

const OutBuf = std.ArrayList(u8);

pub fn main() !void {
    const gpa = std.heap.page_allocator;

    const args = try std.process.argsAlloc(gpa);
    if (args.len < 2) {
        try std.fs.File.stderr().writeAll(
            \\Usage: zir-extract <file.zig>
            \\
            \\Parses a Zig source file, runs AstGen to produce ZIR,
            \\and emits OrganIR JSON on stdout.
            \\
        );
        std.process.exit(1);
    }

    const file_path = args[1];

    // Read source file (sentinel-terminated for the parser)
    const source = std.fs.cwd().readFileAllocOptions(
        gpa,
        file_path,
        10 * 1024 * 1024, // 10 MiB max
        null,
        .@"1",
        0,
    ) catch |err| {
        const msg = try std.fmt.allocPrint(gpa, "zir-extract: cannot read '{s}': {s}\n", .{ file_path, @errorName(err) });
        try std.fs.File.stderr().writeAll(msg);
        std.process.exit(1);
    };

    // Parse to AST
    var tree = try Ast.parse(gpa, source, .zig);
    defer tree.deinit(gpa);

    if (tree.errors.len > 0) {
        try std.fs.File.stderr().writeAll("zir-extract: parse errors in source file\n");
        for (tree.errors) |parse_err| {
            const msg = try std.fmt.allocPrint(gpa, "  error: tag={s}\n", .{@tagName(parse_err.tag)});
            try std.fs.File.stderr().writeAll(msg);
        }
        std.process.exit(1);
    }

    // Run AstGen to produce ZIR
    var zir = try AstGen.generate(gpa, tree);
    defer zir.deinit(gpa);

    // Check for AstGen compile errors
    const compile_errors_idx = @intFromEnum(Zir.ExtraIndex.compile_errors);
    if (zir.extra[compile_errors_idx] != 0) {
        try std.fs.File.stderr().writeAll("zir-extract: AstGen produced compile errors\n");
        std.process.exit(1);
    }

    // Emit OrganIR JSON
    try emitOrganIR(gpa, zir, file_path);
}

fn emitOrganIR(gpa: std.mem.Allocator, zir: Zir, file_path: []const u8) !void {
    const stdout = std.fs.File.stdout();
    var out: OutBuf = .{};

    try out.appendSlice(gpa, "{\n");
    try out.appendSlice(gpa, "  \"schema_version\": \"1.0.0\",\n");
    try out.appendSlice(gpa, "  \"metadata\": {\n");
    try out.appendSlice(gpa, "    \"source_language\": \"zig\",\n");
    try out.appendSlice(gpa, "    \"compiler_version\": \"zig-0.15.2\",\n");
    try out.appendSlice(gpa, "    \"shim_version\": \"0.1.0\",\n");
    try out.appendSlice(gpa, "    \"ir_level\": \"zir\",\n");
    try out.appendSlice(gpa, "    \"ir_note\": \"ZIR is untyped pre-semantic-analysis IR from AstGen\"\n");
    try out.appendSlice(gpa, "  },\n");
    try out.appendSlice(gpa, "  \"module\": {\n");

    try out.appendSlice(gpa, "    \"name\": ");
    try appendJsonString(gpa, &out, file_path);
    try out.appendSlice(gpa, ",\n");
    try out.appendSlice(gpa, "    \"source_file\": ");
    try appendJsonString(gpa, &out, file_path);
    try out.appendSlice(gpa, ",\n");

    const stats = try std.fmt.allocPrint(gpa, "    \"zir_instruction_count\": {d},\n    \"zir_extra_count\": {d},\n    \"zir_string_bytes_count\": {d},\n", .{ zir.instructions.len, zir.extra.len, zir.string_bytes.len });
    try out.appendSlice(gpa, stats);

    // Instructions array
    try out.appendSlice(gpa, "    \"instructions\": [\n");
    const tags = zir.instructions.items(.tag);
    const datas = zir.instructions.items(.data);

    for (tags, 0..) |tag, i| {
        if (i > 0) try out.appendSlice(gpa, ",\n");
        const idx_str = try std.fmt.allocPrint(gpa, "      {{\"index\": {d}, \"tag\": ", .{i});
        try out.appendSlice(gpa, idx_str);
        try appendJsonString(gpa, &out, @tagName(tag));
        try emitInstData(gpa, &out, tag, datas[i], zir);
        try out.appendSlice(gpa, "}");
    }
    try out.appendSlice(gpa, "\n    ],\n");

    // String table
    try out.appendSlice(gpa, "    \"strings\": [\n");
    try emitStringTable(gpa, &out, zir);
    try out.appendSlice(gpa, "    ],\n");

    // Definitions
    try out.appendSlice(gpa, "    \"definitions\": [\n");
    try emitDefinitions(gpa, &out, zir);
    try out.appendSlice(gpa, "    ]\n");

    try out.appendSlice(gpa, "  }\n");
    try out.appendSlice(gpa, "}\n");

    try stdout.writeAll(out.items);
}

/// Determine data field type for a tag using the comptime data_tags map.
const DataField = Zir.Inst.Data.FieldEnum;

fn getDataField(tag: Zir.Inst.Tag) DataField {
    return Zir.Inst.Tag.data_tags[@intFromEnum(tag)];
}

fn emitInstData(
    gpa: std.mem.Allocator,
    out: *OutBuf,
    tag: Zir.Inst.Tag,
    data: Zir.Inst.Data,
    zir: Zir,
) !void {
    try out.appendSlice(gpa, ", \"data\": ");

    const field = getDataField(tag);

    switch (field) {
        .int => {
            const val = try std.fmt.allocPrint(gpa, "{{\"int\": {d}}}", .{data.int});
            try out.appendSlice(gpa, val);
        },

        .float => {
            const val = try std.fmt.allocPrint(gpa, "{{\"float\": {d}}}", .{data.float});
            try out.appendSlice(gpa, val);
        },

        .str => {
            try out.appendSlice(gpa, "{\"str\": ");
            const str_data = data.str;
            const bytes = zir.string_bytes[@intFromEnum(str_data.start)..][0..str_data.len];
            try appendJsonString(gpa, out, bytes);
            try out.appendSlice(gpa, "}");
        },

        .un_node => {
            const un = data.un_node;
            const val = try std.fmt.allocPrint(gpa, "{{\"operand\": {d}}}", .{@intFromEnum(un.operand)});
            try out.appendSlice(gpa, val);
        },

        .un_tok => {
            const un = data.un_tok;
            const val = try std.fmt.allocPrint(gpa, "{{\"operand\": {d}}}", .{@intFromEnum(un.operand)});
            try out.appendSlice(gpa, val);
        },

        .pl_node => {
            // Many pl_node tags have different payload types.
            // Emit what we can based on the specific tag.
            try emitPlNodeData(gpa, out, tag, data.pl_node, zir);
        },

        .pl_tok => {
            try emitPlTokData(gpa, out, tag, data.pl_tok, zir);
        },

        .str_tok => {
            const st = data.str_tok;
            const name = zir.nullTerminatedString(st.start);
            try out.appendSlice(gpa, "{\"str\": ");
            try appendJsonString(gpa, out, name);
            try out.appendSlice(gpa, "}");
        },

        .str_op => {
            // str_op has a NullTerminatedString and a Ref
            try out.appendSlice(gpa, "{}");
        },

        .@"break" => {
            const br = data.@"break";
            const extra = zir.extraData(Zir.Inst.Break, br.payload_index);
            const val = try std.fmt.allocPrint(gpa, "{{\"block_inst\": {d}, \"operand\": {d}}}", .{ @intFromEnum(extra.data.block_inst), @intFromEnum(br.operand) });
            try out.appendSlice(gpa, val);
        },

        .declaration => {
            try out.appendSlice(gpa, "{\"kind\": \"declaration\"}");
        },

        .extended => {
            const ext = data.extended;
            const val = try std.fmt.allocPrint(gpa, "{{\"extended_opcode\": {d}}}", .{@intFromEnum(ext.opcode)});
            try out.appendSlice(gpa, val);
        },

        .dbg_stmt => {
            try out.appendSlice(gpa, "{}");
        },

        .node => {
            try out.appendSlice(gpa, "{}");
        },

        .tok => {
            try out.appendSlice(gpa, "{}");
        },

        .inst_node => {
            try out.appendSlice(gpa, "{}");
        },

        else => {
            try out.appendSlice(gpa, "{}");
        },
    }
}

fn emitPlNodeData(
    gpa: std.mem.Allocator,
    out: *OutBuf,
    tag: Zir.Inst.Tag,
    pl: anytype,
    zir: Zir,
) !void {
    switch (tag) {
        // Binary operations with Bin payload
        .add,
        .addwrap,
        .add_sat,
        .add_unsafe,
        .sub,
        .subwrap,
        .sub_sat,
        .mul,
        .mulwrap,
        .mul_sat,
        .div_exact,
        .div_floor,
        .div_trunc,
        .mod,
        .rem,
        .mod_rem,
        .shl,
        .shl_exact,
        .shl_sat,
        .shr,
        .shr_exact,
        .bit_and,
        .bit_or,
        .bitcast,
        .array_cat,
        .as_node,
        .as_shift_operand,
        .cmp_lt,
        .cmp_lte,
        .cmp_eq,
        .cmp_gte,
        .cmp_gt,
        .cmp_neq,
        .int_from_float,
        .float_from_int,
        .ptr_from_int,
        .enum_from_int,
        .float_cast,
        .int_cast,
        .ptr_cast,
        .truncate,
        .xor,
        .max,
        .min,
        .merge_error_sets,
        => {
            const extra = zir.extraData(Zir.Inst.Bin, pl.payload_index);
            const val = try std.fmt.allocPrint(gpa, "{{\"lhs\": {d}, \"rhs\": {d}}}", .{ @intFromEnum(extra.data.lhs), @intFromEnum(extra.data.rhs) });
            try out.appendSlice(gpa, val);
        },

        // Conditional branch
        .condbr, .condbr_inline => {
            const extra = zir.extraData(Zir.Inst.CondBr, pl.payload_index);
            const val = try std.fmt.allocPrint(gpa, "{{\"condition\": {d}, \"then_body_len\": {d}, \"else_body_len\": {d}}}", .{
                @intFromEnum(extra.data.condition),
                extra.data.then_body_len,
                extra.data.else_body_len,
            });
            try out.appendSlice(gpa, val);
        },

        // Block
        .block, .block_inline, .suspend_block => {
            const extra = zir.extraData(Zir.Inst.Block, pl.payload_index);
            const val = try std.fmt.allocPrint(gpa, "{{\"body_len\": {d}}}", .{extra.data.body_len});
            try out.appendSlice(gpa, val);
        },

        // Block comptime
        .block_comptime => {
            try out.appendSlice(gpa, "{}");
        },

        // Call
        .call => {
            const extra = zir.extraData(Zir.Inst.Call, pl.payload_index);
            const val = try std.fmt.allocPrint(gpa, "{{\"callee\": {d}, \"args_len\": {d}}}", .{ @intFromEnum(extra.data.callee), extra.data.flags.args_len });
            try out.appendSlice(gpa, val);
        },

        // Func
        .func, .func_inferred, .func_fancy => {
            try out.appendSlice(gpa, "{\"kind\": \"function\"}");
        },

        // Everything else: just emit the payload index
        else => {
            const val = try std.fmt.allocPrint(gpa, "{{\"payload_index\": {d}}}", .{pl.payload_index});
            try out.appendSlice(gpa, val);
        },
    }
}

fn emitPlTokData(
    gpa: std.mem.Allocator,
    out: *OutBuf,
    tag: Zir.Inst.Tag,
    pl: anytype,
    zir: Zir,
) !void {
    switch (tag) {
        .param, .param_comptime => {
            const extra = zir.extraData(Zir.Inst.Param, pl.payload_index);
            const name = zir.nullTerminatedString(extra.data.name);
            try out.appendSlice(gpa, "{\"name\": ");
            try appendJsonString(gpa, out, name);
            try out.appendSlice(gpa, "}");
        },
        else => {
            const val = try std.fmt.allocPrint(gpa, "{{\"payload_index\": {d}}}", .{pl.payload_index});
            try out.appendSlice(gpa, val);
        },
    }
}

fn emitStringTable(gpa: std.mem.Allocator, out: *OutBuf, zir: Zir) !void {
    var i: usize = 1; // skip reserved index 0
    var first = true;
    while (i < zir.string_bytes.len) {
        var end = i;
        while (end < zir.string_bytes.len and zir.string_bytes[end] != 0) {
            end += 1;
        }
        if (end > i) {
            if (!first) try out.appendSlice(gpa, ",\n");
            first = false;
            const offset_str = try std.fmt.allocPrint(gpa, "      {{\"offset\": {d}, \"value\": ", .{i});
            try out.appendSlice(gpa, offset_str);
            try appendJsonString(gpa, out, zir.string_bytes[i..end]);
            try out.appendSlice(gpa, "}");
        }
        i = end + 1;
    }
    if (!first) try out.appendSlice(gpa, "\n");
}

fn emitDefinitions(gpa: std.mem.Allocator, out: *OutBuf, zir: Zir) !void {
    const tags = zir.instructions.items(.tag);
    var first = true;

    for (tags, 0..) |tag, i| {
        if (tag != .declaration) continue;

        const inst_idx: Zir.Inst.Index = @enumFromInt(i);
        const decl = zir.getDeclaration(inst_idx);

        if (!first) try out.appendSlice(gpa, ",\n");
        first = false;

        // Resolve name
        const name_str: []const u8 = if (decl.name != .empty)
            zir.nullTerminatedString(decl.name)
        else switch (decl.kind) {
            .unnamed_test => "(unnamed test)",
            .@"comptime" => "(comptime)",
            else => "(unknown)",
        };

        try out.appendSlice(gpa, "      {\n");
        try out.appendSlice(gpa, "        \"name\": {\"module\": \"\", \"text\": ");
        try appendJsonString(gpa, out, name_str);
        const idx_str = try std.fmt.allocPrint(gpa, ", \"unique\": {d}", .{i});
        try out.appendSlice(gpa, idx_str);
        try out.appendSlice(gpa, "},\n");
        try out.appendSlice(gpa, "        \"type\": {\"tag\": \"any\"},\n");

        // Value body instructions
        try out.appendSlice(gpa, "        \"expr\": {\"tag\": \"zir_body\", \"instructions\": [");
        if (decl.value_body) |vb| {
            for (vb, 0..) |inst, j| {
                if (j > 0) try out.appendSlice(gpa, ", ");
                const j_str = try std.fmt.allocPrint(gpa, "{d}", .{@intFromEnum(inst)});
                try out.appendSlice(gpa, j_str);
            }
        }
        try out.appendSlice(gpa, "]},\n");

        // Sort: check if value body contains a func instruction
        var sort: []const u8 = "val";
        if (decl.value_body) |vb| {
            for (vb) |inst| {
                const t = tags[@intFromEnum(inst)];
                if (t == .func or t == .func_inferred or t == .func_fancy) {
                    sort = "fun";
                    break;
                }
            }
        }
        try out.appendSlice(gpa, "        \"sort\": ");
        try appendJsonString(gpa, out, sort);
        try out.appendSlice(gpa, ",\n");

        try out.appendSlice(gpa, "        \"visibility\": ");
        if (decl.is_pub) {
            try out.appendSlice(gpa, "\"public\"\n");
        } else {
            try out.appendSlice(gpa, "\"private\"\n");
        }
        try out.appendSlice(gpa, "      }");
    }
    if (!first) try out.appendSlice(gpa, "\n");
}

fn appendJsonString(gpa: std.mem.Allocator, out: *OutBuf, s: []const u8) !void {
    try out.append(gpa, '"');
    for (s) |c| {
        switch (c) {
            '"' => try out.appendSlice(gpa, "\\\""),
            '\\' => try out.appendSlice(gpa, "\\\\"),
            '\n' => try out.appendSlice(gpa, "\\n"),
            '\r' => try out.appendSlice(gpa, "\\r"),
            '\t' => try out.appendSlice(gpa, "\\t"),
            else => {
                if (c < 0x20) {
                    const esc = try std.fmt.allocPrint(gpa, "\\u{x:0>4}", .{c});
                    try out.appendSlice(gpa, esc);
                } else {
                    try out.append(gpa, c);
                }
            },
        }
    }
    try out.append(gpa, '"');
}
