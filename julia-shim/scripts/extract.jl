#!/usr/bin/env julia
#
# extract.jl — Julia typed IR extraction for OrganIR
#
# Usage: julia extract.jl <file.jl>
#
# Includes the target file into Main, discovers user-defined functions,
# calls code_typed() on each, and emits OrganIR JSON on stdout.
#
# Limitations:
#   - Functions without type annotations use Any, giving less useful IR.
#   - Only functions defined at module level in Main are discovered.
#   - Generic functions with multiple methods emit one definition per method.

if length(ARGS) < 1
    println(stderr, "Usage: julia extract.jl <file.jl>")
    exit(1)
end

const input_file = ARGS[1]

if !isfile(input_file)
    println(stderr, "Error: file not found: $input_file")
    exit(1)
end

# --- JSON helpers (no external packages) ---

function json_escape(s::AbstractString)::String
    buf = IOBuffer()
    for c in s
        if c == '"'
            write(buf, "\\\"")
        elseif c == '\\'
            write(buf, "\\\\")
        elseif c == '\n'
            write(buf, "\\n")
        elseif c == '\r'
            write(buf, "\\r")
        elseif c == '\t'
            write(buf, "\\t")
        else
            write(buf, c)
        end
    end
    return String(take!(buf))
end

json_str(s) = "\"$(json_escape(string(s)))\""

function json_array(items::Vector{String})::String
    return "[" * join(items, ", ") * "]"
end

# --- IR node translation ---

function translate_value(v)::String
    if v isa Core.SSAValue
        return "{\"evar\": {\"text\": \"%$(v.id)\", \"unique\": $(v.id)}}"
    elseif v isa Core.Argument
        return "{\"evar\": {\"text\": \"_$(v.n)\", \"unique\": $(- v.n)}}"
    elseif v isa GlobalRef
        return "{\"evar\": {\"text\": $(json_str(string(v.name))), \"unique\": 0}}"
    elseif v isa QuoteNode
        return "{\"elit\": {\"string\": $(json_str(string(v.value)))}}"
    elseif v isa Int64 || v isa Int32 || v isa Int
        return "{\"elit\": {\"int\": $v}}"
    elseif v isa UInt64 || v isa UInt32
        return "{\"elit\": {\"int\": $(Int64(v))}}"
    elseif v isa Float64 || v isa Float32
        return "{\"elit\": {\"float\": $v}}"
    elseif v isa Bool
        return "{\"elit\": {\"int\": $(v ? 1 : 0)}}"
    elseif v isa String
        return "{\"elit\": {\"string\": $(json_str(v))}}"
    elseif v isa Symbol
        return "{\"evar\": {\"text\": $(json_str(string(v))), \"unique\": 0}}"
    elseif v === nothing
        return "{\"elit\": {\"int\": 0}}"
    else
        return "{\"elit\": {\"string\": $(json_str(string(v)))}}"
    end
end

function translate_type(t)::String
    name = string(t)
    return "{\"con\": {\"qname\": {\"module\": \"julia\", \"name\": {\"text\": $(json_str(name))}}}}"
end

function translate_stmt(stmt, idx::Int)::Union{String, Nothing}
    node = if stmt isa Expr
        if stmt.head === :call
            fn = translate_value(stmt.args[1])
            args = [translate_value(a) for a in stmt.args[2:end]]
            "{\"eapp\": {\"fn\": $fn, \"args\": $(json_array(args))}}"
        elseif stmt.head === :invoke
            # (:invoke, MethodInstance, f, args...)
            fn = translate_value(stmt.args[2])
            args = [translate_value(a) for a in stmt.args[3:end]]
            "{\"eapp\": {\"fn\": $fn, \"args\": $(json_array(args))}}"
        elseif stmt.head === :new
            args = [translate_value(a) for a in stmt.args]
            "{\"eapp\": {\"fn\": {\"evar\": {\"text\": \"new\", \"unique\": 0}}, \"args\": $(json_array(args))}}"
        elseif stmt.head === :(=)
            # Assignment — translate the RHS
            translate_value(stmt.args[2])
        elseif stmt.head === :boundscheck
            "{\"elit\": {\"string\": \"boundscheck\"}}"
        else
            "{\"elit\": {\"string\": $(json_str("expr:" * string(stmt.head)))}}"
        end
    elseif stmt isa Core.GotoIfNot
        cond = translate_value(stmt.cond)
        "{\"ssa_gotoifnot\": {\"cond\": $cond, \"dest\": $(stmt.dest)}}"
    elseif stmt isa Core.GotoNode
        "{\"ssa_goto\": {\"dest\": $(stmt.label)}}"
    elseif stmt isa Core.ReturnNode
        if isdefined(stmt, :val)
            val = translate_value(stmt.val)
            "{\"ssa_return\": {\"value\": $val}}"
        else
            # Unreachable return
            "{\"ssa_return\": {\"value\": {\"elit\": {\"string\": \"unreachable\"}}}}"
        end
    elseif stmt isa Core.PhiNode
        edges = [string(e) for e in stmt.edges]
        vals = [isdefined(stmt.values, i) ? translate_value(stmt.values[i]) : "{\"elit\": {\"string\": \"undef\"}}" for i in 1:length(stmt.values)]
        "{\"ssa_phi\": {\"edges\": [$(join(edges, ", "))], \"values\": $(json_array(vals))}}"
    elseif stmt isa Core.PiNode
        val = translate_value(stmt.val)
        typ = json_str(string(stmt.typ))
        "{\"ssa_pi\": {\"value\": $val, \"type\": $typ}}"
    elseif stmt === nothing
        return nothing  # nop, skip
    else
        # Literal or other value in statement position
        translate_value(stmt)
    end

    if node === nothing
        return nothing
    end

    return "{\"index\": $idx, \"node\": $node}"
end

function translate_type_for_organir(t)::String
    return "{\"con\": {\"qname\": {\"module\": \"julia\", \"name\": {\"text\": $(json_str(string(t)))}}}}"
end

function make_fn_type(param_types, ret_type)::String
    args = ["{\"multiplicity\": \"many\", \"type\": $(translate_type_for_organir(t))}" for t in param_types]
    result = translate_type_for_organir(ret_type)
    return "{\"fn\": {\"args\": $(json_array(args)), \"effect\": {\"effects\": []}, \"result\": $result}}"
end

# --- Main extraction ---

# Record names before include so we can filter
const names_before = Set(names(Main; all=false, imported=false))

# Include the target file
try
    Base.include(Main, input_file)
catch e
    println(stderr, "Error loading $input_file: $e")
    exit(1)
end

# Names introduced by this script itself (must be excluded)
const _script_names = Set([:names_before, :names_after, :user_names,
    :definitions, :unique_counter, :next_unique, :input_file,
    :json_escape, :json_str, :json_array, :translate_value,
    :translate_type, :translate_stmt, :translate_type_for_organir,
    :make_fn_type, :_script_names])

const names_after = names(Main; all=false, imported=false)
const user_names = filter(n -> !(n in names_before) && !(n in _script_names) && n != :eval && n != :include, names_after)

definitions = String[]
unique_counter = Ref(100)

function next_unique()::Int
    unique_counter[] += 1
    return unique_counter[]
end

for sym in user_names
    obj = try
        getfield(Main, sym)
    catch
        continue
    end

    if !(obj isa Function)
        continue
    end

    for m in methods(obj)
        # Extract parameter types from method signature
        sig_params = m.sig.parameters
        # sig_params[1] is typeof(f), rest are argument types

        # Get typed IR
        typed_results = try
            code_typed(obj, Tuple{sig_params[2:end]...})
        catch e
            # If code_typed fails (e.g., abstract types), skip
            continue
        end

        for (ci, ret_type) in typed_results
            # ci is a CodeInfo object
            stmts = String[]
            for (i, stmt) in enumerate(ci.code)
                s = translate_stmt(stmt, i)
                if s !== nothing
                    push!(stmts, s)
                end
            end

            # SSA value types
            ssa_types = String[]
            if isdefined(ci, :ssavaluetypes) && ci.ssavaluetypes isa Vector
                for (i, t) in enumerate(ci.ssavaluetypes)
                    push!(ssa_types, "{\"index\": $i, \"type\": $(translate_type_for_organir(t))}")
                end
            end

            # Slot names and types (parameters + locals)
            slots = String[]
            if isdefined(ci, :slotnames)
                for (i, name) in enumerate(ci.slotnames)
                    slottype = if isdefined(ci, :slottypes) && i <= length(ci.slottypes)
                        ci.slottypes[i]
                    else
                        Any
                    end
                    push!(slots, "{\"index\": $i, \"name\": $(json_str(string(name))), \"type\": $(translate_type_for_organir(slottype))}")
                end
            end

            # Build parameter types for function type (skip #self#)
            fn_param_types = sig_params[2:end]

            fn_unique = next_unique()
            fn_name = string(sym)
            arity = length(fn_param_types)

            body = """{
        "ssa_body": {
          "stmts": $(json_array(stmts)),
          "ssa_types": $(json_array(ssa_types)),
          "slots": $(json_array(slots))
        }
      }"""

            def = """{
      "name": {"module": "Main", "name": {"text": $(json_str(fn_name)), "unique": $fn_unique}},
      "type": $(make_fn_type(fn_param_types, ret_type)),
      "expr": $body,
      "sort": "fun",
      "visibility": "public",
      "arity": $arity
    }"""
            push!(definitions, def)
        end
    end
end

# Emit the full OrganIR envelope
compiler_version = "julia-$(VERSION)"

json_output = """{
  "schema_version": "1.0.0",
  "metadata": {
    "source_language": "julia",
    "compiler_version": $(json_str(compiler_version)),
    "source_file": $(json_str(input_file)),
    "shim_version": "0.1.0"
  },
  "module": {
    "name": "Main",
    "exports": $(json_array([json_str(string(n)) for n in user_names])),
    "definitions": [
    $(join(definitions, ",\n    "))
  ],
    "data_types": [],
    "effect_decls": []
  }
}
"""

print(json_output)
