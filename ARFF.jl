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
    mode, start, len, fields = normal, 0, length(s), String[]

    for (ch, idx) in enumerate(s)
        if mode == normal
            if ch == '\''
                mode = ticks
            elseif ch == '\"'
                mode = quotes
            else
                if ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n'
                    mode = space
                    push!(fields, s[start:idx-1])
                elseif idx == len
                    push!(fields, s[start:idx])
                end
            end
        elseif mode == space
            if ch >= '!' || ch <= '~'
                start = idx
                if ch == '\''
                    mode = ticks
                elseif ch == '\"'
                    mode = quotes
                else
                    mode = normal
                end
            end
        elseif mode == ticks
            if ch == '\''
                mode = normal
            end
        elseif mode == quotes
            if ch == '\"'
                mode = normal
            end
        else
            error("Unknown mode: $mode")
        end
    end
    return fields
end

function create_validate_attribute(fields::Vector{String})
    len = length(fields)
    valid = (len == 3 || len == 4)

    if valid
        name = fields[2]
        if fields[3] == "NUMERIC" || fields[3] == "numeric"
            return NumericAttribute(name, Float64), valid
        elseif fields[3] == "INTEGER" || fields[3] == "integer"
            return IntegerAttribute(name, Int64), valid
        elseif fields[3] == "REAL" || fields[3] == "real"
            return RealAttribute(name, Float64), valid
        elseif fields[3] == "STRING" || fields[3] == "string"
            return StringAttribute(name, String), valid
        elseif fields[3] == "DATE" || fields[3] == "date"
            if len == 4
                return DateAttribute(name, String, fields[3]), valid
            end
            return DateAttribute(name, String, ""), valid
        elseif fields[3] == "@RELATIONAL" || fields[3] == "@relational"
            println("Note :: Relational attributes are not yet supported")
            return Attribute(), !valid
        else
            field = fields[3]
            valid = valid && field[1] == '{' && field[end] == '}' && occursin(",", field)
            if valid
                values = Dict{String,Int}()
                for (idx, value) in enumerate(split(field[2:end-1], ","))
                    values[strip(value)] = idx
                end
                return NominalAttribute(name, Int, values), valid
            end
        end
    end
end

function read_arff(file::String)::Relation
    relation_name = ""
    attributes = Vector{Attribute}()
    data = Vector{Any}()

    open(file) do f
        for line in eachline(f)
            line = strip(line)
            line[1] == '%' && continue
            line[1] != '@' && continue
            fields = arff_split(line)
            if length(fields) > 0
                if fields[1] == "@relation" || fields[1] == "@RELATION"
                    relation_name = fields[2]
                elseif fields[1] == "@attribute" || fields[1] == "@ATTRIBUTE"
                    attribute, valid = create_validate_attribute(fields)
                    if valid
                        push!(attributes, attribute)
                    else
                        println("Error :: Invalid attribute: $fields")
                    end
                elseif fields[1] == "@data" || fields[1] == "@DATA"
                    break
                else
                    println("Warning :: Unknown field: $fields")
                    exit()
                end
            end
        end

        for line in eachline(f)
            values = split(line, ",")
        end
    end

    return Relation(relation_name, attributes, data)
end

end # module ARFF
