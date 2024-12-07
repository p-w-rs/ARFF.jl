module ARFF

using Dates

abstract type Attribute end

struct NumericAttribute <: Attribute
    name::String
    type::DataType
end

struct IntegerAttribute <: Attribute
    name::String
    type::DataType
end

struct RealAttribute <: Attribute
    name::String
    type::DataType
end

struct NominalAttribute <: Attribute
    name::String
    type::DataType
    values::Dict{String,Int}
end

struct StringAttribute <: Attribute
    name::String
    type::DataType
end

struct DateAttribute <: Attribute
    name::String
    type::DataType
    format::String
end

struct Relation
    name::String
    attributes::Vector{Attribute}
    data::Vector{Any}
end

function arff_split(s::String)::Vector{String}
    @enum Mode begin
        normal
        space
        ticks
        quotes
    end

    mode, start, len = normal, 1, length(s)
    fields = String[]

    i = 1
    while i ≤ len
        ch = s[i]

        if mode == normal
            if ch == '\''
                start = i
                mode = ticks
            elseif ch == '"'
                start = i
                mode = quotes
            elseif ch in [' ', '\t', '\r', '\n']
                if i > start
                    push!(fields, strip(s[start:i-1]))
                end
                mode = space
                start = i + 1
            elseif i == len
                push!(fields, strip(s[start:i]))
            end
        elseif mode == space
            if !isspace(ch)
                start = i
                if ch == '\''
                    mode = ticks
                elseif ch == '"'
                    mode = quotes
                else
                    mode = normal
                end
            end
        elseif mode == ticks
            if ch == '\''
                push!(fields, strip(s[start:i], ['\'']))
                mode = space
                start = i + 1
            end
        elseif mode == quotes
            if ch == '"'
                push!(fields, strip(s[start:i], ['"']))
                mode = space
                start = i + 1
            end
        end
        i += 1
    end

    # Handle case where string ends in a non-space character
    if mode == normal && start ≤ len
        push!(fields, strip(s[start:end]))
    end

    return filter(!isempty, fields)
end

function create_validate_attribute(fields::Vector{String})::Tuple{Union{Attribute,Nothing},Bool}
    len = length(fields)
    if len < 3
        return nothing, false
    end

    name = strip(fields[2], ['"', '\''])
    type_str = uppercase(strip(fields[3]))

    try
        if type_str in ["NUMERIC", "INTEGER", "REAL", "STRING", "DATE"]
            if type_str == "NUMERIC"
                return NumericAttribute(name, Float64), true
            elseif type_str == "INTEGER"
                return IntegerAttribute(name, Int64), true
            elseif type_str == "REAL"
                return RealAttribute(name, Float64), true
            elseif type_str == "STRING"
                return StringAttribute(name, String), true
            elseif type_str == "DATE"
                format = len ≥ 4 ? strip(fields[4], ['"', '\'']) : "yyyy-mm-dd"
                return DateAttribute(name, DateTime, format), true
            end
        elseif startswith(type_str, "{") && endswith(type_str, "}")
            # Handle nominal attributes
            values_str = type_str[2:end-1]
            values = Dict{String,Int}()
            for (idx, value) in enumerate(split(values_str, ","))
                clean_value = strip(strip(value), ['"', '\''])
                values[clean_value] = idx
            end
            return NominalAttribute(name, Int, values), true
        else
            @warn "Unsupported attribute type: $type_str"
            return nothing, false
        end
    catch e
        @error "Error creating attribute" exception = (e, catch_backtrace())
        return nothing, false
    end
end

function parse_value(value::String, attribute::NumericAttribute)
    value = strip(value)
    return parse(Float64, value)
end

function parse_value(value::String, attribute::IntegerAttribute)
    value = strip(value)
    return parse(Int64, value)
end

function parse_value(value::String, attribute::RealAttribute)
    value = strip(value)
    return parse(Float64, value)
end

function parse_value(value::String, attribute::NominalAttribute)
    value = strip(strip(value), ['"', '\''])
    return get(attribute.values, value, missing)
end

function parse_value(value::String, attribute::StringAttribute)
    return strip(strip(value), ['"', '\''])
end

function parse_value(value::String, attribute::DateAttribute)
    value = strip(strip(value), ['"', '\''])
    try
        if isempty(attribute.format)
            return DateTime(value)
        else
            return DateTime(value, DateFormat(attribute.format))
        end
    catch e
        @warn "Error parsing date value: $value with format: $(attribute.format)"
        return missing
    end
end

function split_data_line(line::String)::Vector{String}
    values = String[]
    current = ""
    in_quotes = false
    quote_char = nothing
    escaped = false

    for char in line
        if escaped
            current *= char
            escaped = false
            continue
        end

        if char == '\\'
            escaped = true
            continue
        end

        if char in ['"', '\'']
            if isnothing(quote_char)
                in_quotes = true
                quote_char = char
            elseif char == quote_char
                in_quotes = false
                quote_char = nothing
            else
                current *= char
            end
        elseif char == ',' && !in_quotes
            push!(values, current)
            current = ""
        else
            current *= char
        end
    end

    push!(values, current)
    return map(strip, values)
end

function find_class_attribute(attributes::Vector{Attribute})::Int
    class_names = ["class", "target", "label", "outcome"]

    # Try to find attribute with common class names
    for (idx, attr) in enumerate(attributes)
        if lowercase(attr.name) in class_names
            return idx
        end
    end

    # If no common class name found, assume last attribute is class
    return length(attributes)
end

function read_arff(file::String; class_index::Union{Int,Nothing}=nothing)
    if !isfile(file)
        throw(ArgumentError("File not found: $file"))
    end

    relation_name = ""
    attributes = Vector{Attribute}()
    raw_data = Vector{Vector{Any}}()

    open(file) do f
        data_section = false
        sparse_data = false

        for (line_num, line) in enumerate(eachline(f))
            try
                line = strip(line)
                isempty(line) && continue
                startswith(line, '%') && continue

                if !data_section
                    startswith(line, '@') || continue
                    fields = arff_split(line)
                    isempty(fields) && continue

                    cmd = lowercase(fields[1])
                    if cmd == "@relation"
                        relation_name = strip(fields[2], ['"', '\''])
                    elseif cmd == "@attribute"
                        attribute, valid = create_validate_attribute(fields)
                        if valid && !isnothing(attribute)
                            push!(attributes, attribute)
                        else
                            @warn "Invalid attribute definition at line $line_num: $line"
                        end
                    elseif cmd == "@data"
                        data_section = true
                        sparse_data = contains(line, "{") && contains(line, "}")
                    end
                    continue
                end

                # Process data section
                line = strip(line)
                startswith(line, '%') && continue
                isempty(line) && continue

                values = if sparse_data
                    parse_sparse_data(line, length(attributes))
                else
                    split_data_line(line)
                end

                if length(values) == length(attributes)
                    parsed_values = Any[]
                    for (value, attr) in zip(values, attributes)
                        if value == "?" || isempty(strip(value))
                            push!(parsed_values, missing)
                        else
                            try
                                push!(parsed_values, parse_value(value, attr))
                            catch e
                                @warn "Error parsing value '$value' for attribute $(attr.name) at line $line_num: $e"
                                push!(parsed_values, missing)
                            end
                        end
                    end
                    push!(raw_data, parsed_values)
                else
                    @warn "Skipping line $line_num: incorrect number of values"
                end
            catch e
                @error "Error processing line $line_num" exception = (e, catch_backtrace())
            end
        end
    end

    if isempty(raw_data)
        throw(ErrorException("No valid data found in file"))
    end

    # Determine class index
    class_idx = isnothing(class_index) ? find_class_attribute(attributes) : class_index
    if class_idx > length(attributes)
        throw(ArgumentError("Class index out of bounds"))
    end

    # Convert to matrix format
    n_samples = length(raw_data)
    n_features = length(attributes)

    # Prepare X and y
    X = Matrix{Any}(undef, n_samples, n_features - 1)
    y = Vector{Any}(undef, n_samples)

    for i in 1:n_samples
        feature_idx = 1
        for j in 1:n_features
            if j == class_idx
                y[i] = raw_data[i][j]
            else
                X[i, feature_idx] = raw_data[i][j]
                feature_idx += 1
            end
        end
    end

    return X, y, Relation(relation_name, attributes, raw_data)
end

function parse_sparse_data(line::String, n_attributes::Int)::Vector{String}
    values = fill("0", n_attributes)
    content = match(r"\{(.*)\}", line)

    if isnothing(content)
        @warn "Invalid sparse data format: $line"
        return values
    end

    for pair in split(content.captures[1], ",")
        pair = strip(pair)
        isempty(pair) && continue

        idx_val = split(pair, " ")
        if length(idx_val) == 2
            idx = parse(Int, idx_val[1]) + 1  # Convert to 1-based indexing
            if 1 ≤ idx ≤ n_attributes
                values[idx] = strip(idx_val[2])
            end
        end
    end

    return values
end

end # module ARFF
