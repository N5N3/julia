# if !isdefined(Main, :printrec)
function printrec(rec)
    for i in eachindex(rec)
        if !isassigned(rec, i)
            rec[i] = missing
        end
    end
    rec = permutedims(reshape(rec, M, :));
    a = findfirst(isnothing, rec[:,1])
    if a isa Int
        rec = rec[1:a-1,:]
    end
    function get_entry(a)
        a == 1 ?  "intersect" :
        a == 2 ?  "subtype_by_bounds" :
        a == 3 ?  "subtype_in_env" :
        a == 4 ?  "subtype" :
        a == 5 ?  "intersect_var" :
        a == 6 ?  "var_lt" :
        a == 7 ?  "var_gt" :
        a == 8 ?  "intersect_invariant" :
        a == 9 ?  "intersect_aside" :
        a == 10 ? "subtype_in_env_existential" :
        a == 11 ? "try_subtype_in_env" :
        a == 12 ? "fresh_subtype_in_env" :
        a == 13 ? "intersect_unionall" :
        a == 14 ? "_intersect_unionall" :
        a == 15 ? "final_unionall" :
        a == 16 ? "reachable_var" :
        a == 114105 ? "merge_env" :
        a
    end
    function expand_env(env::Vector{Any})
        iter = Iterators.map(enumerate(env)) do (i, x)
            name, lb, ub, con, oc, oi = x
            if lb === Union{} && ub === Any
                "[$i($con,$oc,$oi)]: $name"
            elseif lb === ub
                "[$i($con,$oc,$oi)]: $(name)=$(replace_free_var(lb))"
            elseif lb === Union{}
                "[$i($con,$oc,$oi)]: $(name)<:$(replace_free_var(ub))"
            elseif ub === Any
                "[$i($con,$oc,$oi)]: $(name)>:$(replace_free_var(lb))"
            else
                "[$i($con,$oc,$oi)]: $(name)<$(replace_free_var(lb)),$(replace_free_var(ub))>"
            end
        end |> collect
        return join(iter, ", ")
    end
    max_stack = size(rec, 1)
    out_str = similar(rec, String, max_stack)
    N = length(string(max_stack))
    # replace_free_var(T) = T
    function replace_free_var(@nospecialize(T), env = Dict())
        return if T isa UnionAll
            var = TypeVar(T.var.name,
                          replace_free_var(T.var.lb, env),
                          replace_free_var(T.var.ub, env))
            env[T.var] = var
            To = replace_free_var(T.body, env)
            delete!(env, T.var)
            UnionAll(var, To)
        elseif T isa Union
            Union{replace_free_var(T.a, env), replace_free_var(T.b, env)}
        elseif T isa Core.TypeofVararg && isdefined(T, :T) && isdefined(T, :N)
            Vararg{replace_free_var(T.T, env), replace_free_var(T.N, env)}
        elseif T isa Core.TypeofVararg && isdefined(T, :T)
            Vararg{replace_free_var(T.T, env)}
        elseif T isa DataType && length(T.parameters) > 0
            Core.apply_type(T.name.wrapper, map(Base.Fix2(replace_free_var, env), (T.parameters...,))...)
        elseif T isa TypeVar
            get(env, T, TypeVar(T.name))
        else
            T
        end
    end


    for i in axes(rec, 1)
        from, to, id, extra, x, y, z, env, env2 = rec[i, :]
        if all(==(nothing), (from, to, id, extra, x, y, z, env, env2))
            out_str[i] = ""
            continue
        end
        if from !== nothing
            from = string(from, pad = N)
        end
        if to !== nothing
            to = string(to, pad = N)
        end
        if env isa Vector{Any}
            env = expand_env(env)
        end
        if env2 isa Vector{Any}
            env2 = expand_env(env2)
        end
        empty = ' '^(2N+4)
        out_str[i] = if env !== nothing
            "$from->$to: $(get_entry(id))($extra) \t $(replace_free_var(z))
                $(replace_free_var(x))\t\t\t\t$(replace_free_var(y))
                $env
                $env2"
        else
            "$from->$to: $(get_entry(id))($extra) \t $z
                $(replace_free_var(x))\t\t\t\t$(replace_free_var(y))
                $env2"
        end
    end
    open(raw"C:\Users\MYJ\Documents\GitHub\julia\test\Untitled-101.jl", "w") do fid
        write(fid, join(out_str,"\n---------------------------------------\n"))
    end
# end
end


S = Tuple{L, Tuple{Int64}, Int64} where L<:Union{Int64, Tuple{Int64}}
T = Tuple{Union{LT, LT1}, Union{R, R1}, Int64} where {R<:Tuple{Int64}, LT<:Int64, R1<:Tuple{Int64}, LT1<:R1}

# S, T = T, S
M = 9
rec = Any[nothing for _ in 1:5000M];
try
    # @ccall jl_intersect_toy(S::Any, T::Any, rec::Ptr{Any}, (5000M)::Cint, 0::Cint)::Any
    @ccall jl_subtype_env_toy(S::Any, T::Any, rec::Ptr{Any}, (5000M)::Cint, 0::Cint)::Cint
catch
    printrec(rec)
end
printrec(rec)