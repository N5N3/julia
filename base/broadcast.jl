# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Base.Broadcast

Module containing the broadcasting implementation.
"""
module Broadcast

using .Base.Cartesian
using .Base: Indices, OneTo, tail, to_shape, isoperator, promote_typejoin, promote_typejoin_union, @pure,
             _msk_end, unsafe_bitgetindex, bitcache_chunks, bitcache_size, dumpbitcache, unalias, negate
import .Base: copy, copyto!, axes, @propagate_inbounds
export broadcast, broadcast!, BroadcastStyle, broadcast_axes, broadcastable, dotview, @__dot__, BroadcastFunction

## Computing the result's axes: deprecated name
const broadcast_axes = axes

### Objects with customized broadcasting behavior should declare a BroadcastStyle

"""
`BroadcastStyle` is an abstract type and trait-function used to determine behavior of
objects under broadcasting. `BroadcastStyle(typeof(x))` returns the style associated
with `x`. To customize the broadcasting behavior of a type, one can declare a style
by defining a type/method pair

    struct MyContainerStyle <: BroadcastStyle end
    Base.BroadcastStyle(::Type{<:MyContainer}) = MyContainerStyle()

One then writes method(s) (at least [`similar`](@ref)) operating on
`Broadcasted{MyContainerStyle}`. There are also several pre-defined subtypes of `BroadcastStyle`
that you may be able to leverage; see the
[Interfaces chapter](@ref man-interfaces-broadcasting) for more information.
"""
abstract type BroadcastStyle end

"""
`Broadcast.Style{C}()` defines a [`BroadcastStyle`](@ref) signaling through the type
parameter `C`. You can use this as an alternative to creating custom subtypes of `BroadcastStyle`,
for example

    Base.BroadcastStyle(::Type{<:MyContainer}) = Broadcast.Style{MyContainer}()
"""
struct Style{T} <: BroadcastStyle end

BroadcastStyle(::Type{<:Tuple}) = Style{Tuple}()

struct Unknown <: BroadcastStyle end
BroadcastStyle(::Type{Union{}}) = Unknown()  # ambiguity resolution

"""
`Broadcast.AbstractArrayStyle{N} <: BroadcastStyle` is the abstract supertype for any style
associated with an `AbstractArray` type.
The `N` parameter is the dimensionality, which can be handy for AbstractArray types
that only support specific dimensionalities:

    struct SparseMatrixStyle <: Broadcast.AbstractArrayStyle{2} end
    Base.BroadcastStyle(::Type{<:SparseMatrixCSC}) = SparseMatrixStyle()

For `AbstractArray` types that support arbitrary dimensionality, `N` can be set to `Any`:

    struct MyArrayStyle <: Broadcast.AbstractArrayStyle{Any} end
    Base.BroadcastStyle(::Type{<:MyArray}) = MyArrayStyle()

In cases where you want to be able to mix multiple `AbstractArrayStyle`s and keep track
of dimensionality, your style needs to support a [`Val`](@ref) constructor:

    struct MyArrayStyleDim{N} <: Broadcast.AbstractArrayStyle{N} end
    (::Type{<:MyArrayStyleDim})(::Val{N}) where {N} = MyArrayStyleDim{N}()

Note that if two or more `AbstractArrayStyle` subtypes conflict, broadcasting machinery
will fall back to producing `Array`s. If this is undesirable, you may need to
define binary [`BroadcastStyle`](@ref) rules to control the output type.

See also [`Broadcast.DefaultArrayStyle`](@ref).
"""
abstract type AbstractArrayStyle{N} <: BroadcastStyle end

"""
`Broadcast.ArrayStyle{MyArrayType}()` is a [`BroadcastStyle`](@ref) indicating that an object
behaves as an array for broadcasting. It presents a simple way to construct
[`Broadcast.AbstractArrayStyle`](@ref)s for specific `AbstractArray` container types.
Broadcast styles created this way lose track of dimensionality; if keeping track is important
for your type, you should create your own custom [`Broadcast.AbstractArrayStyle`](@ref).
"""
struct ArrayStyle{A<:AbstractArray} <: AbstractArrayStyle{Any} end
ArrayStyle{A}(::Val) where A = ArrayStyle{A}()

"""
`Broadcast.DefaultArrayStyle{N}()` is a [`BroadcastStyle`](@ref) indicating that an object
behaves as an `N`-dimensional array for broadcasting. Specifically, `DefaultArrayStyle` is
used for any
`AbstractArray` type that hasn't defined a specialized style, and in the absence of
overrides from other `broadcast` arguments the resulting output type is `Array`.
When there are multiple inputs to `broadcast`, `DefaultArrayStyle` "loses" to any other [`Broadcast.ArrayStyle`](@ref).
"""
struct DefaultArrayStyle{N} <: AbstractArrayStyle{N} end
DefaultArrayStyle(::Val{N}) where {N} = DefaultArrayStyle{N}()
DefaultArrayStyle{M}(::Val{N}) where {N,M} = DefaultArrayStyle{N}()
const DefaultVectorStyle = DefaultArrayStyle{1}
const DefaultMatrixStyle = DefaultArrayStyle{2}
BroadcastStyle(::Type{<:AbstractArray{T,N}}) where {T,N} = DefaultArrayStyle{N}()
BroadcastStyle(::Type{T}) where {T} = DefaultArrayStyle{ndims(T)}()

# `ArrayConflict` is an internal type signaling that two or more different `AbstractArrayStyle`
# objects were supplied as arguments, and that no rule was defined for resolving the
# conflict. The resulting output is `Array`. While this is the same output type
# produced by `DefaultArrayStyle`, `ArrayConflict` "poisons" the BroadcastStyle so that
# 3 or more arguments still return an `ArrayConflict`.
struct ArrayConflict <: AbstractArrayStyle{Any} end
ArrayConflict(::Val) = ArrayConflict()

### Binary BroadcastStyle rules
"""
    BroadcastStyle(::Style1, ::Style2) = Style3()

Indicate how to resolve different `BroadcastStyle`s. For example,

    BroadcastStyle(::Primary, ::Secondary) = Primary()

would indicate that style `Primary` has precedence over `Secondary`.
You do not have to (and generally should not) define both argument orders.
The result does not have to be one of the input arguments, it could be a third type.

Please see the [Interfaces chapter](@ref man-interfaces-broadcasting) of the manual for
more information.
"""
BroadcastStyle(::S, ::S) where S<:BroadcastStyle = S() # homogeneous types preserved
# Fall back to Unknown. This is necessary to implement argument-swapping
BroadcastStyle(::BroadcastStyle, ::BroadcastStyle) = Unknown()
# Unknown loses to everything
BroadcastStyle(::Unknown, ::Unknown) = Unknown()
BroadcastStyle(::S, ::Unknown) where S<:BroadcastStyle = S()
# Precedence rules
BroadcastStyle(a::AbstractArrayStyle{0}, b::Style{Tuple}) = b
BroadcastStyle(a::AbstractArrayStyle, ::Style{Tuple})    = a
BroadcastStyle(::A, ::A) where A<:ArrayStyle             = A()
BroadcastStyle(::ArrayStyle, ::ArrayStyle)               = Unknown()
BroadcastStyle(::A, ::A) where A<:AbstractArrayStyle     = A()
Base.@pure function BroadcastStyle(a::A, b::B) where {A<:AbstractArrayStyle{M},B<:AbstractArrayStyle{N}} where {M,N}
    if Base.typename(A) === Base.typename(B)
        return A(Val(max(M, N)))
    end
    return Unknown()
end
# Any specific array type beats DefaultArrayStyle
BroadcastStyle(a::AbstractArrayStyle{Any}, ::DefaultArrayStyle) = a
BroadcastStyle(a::AbstractArrayStyle{N}, ::DefaultArrayStyle{N}) where {N} = a
BroadcastStyle(a::AbstractArrayStyle{M}, ::DefaultArrayStyle{N}) where {M,N} =
    typeof(a)(Val(max(M, N)))

### Lazy-wrapper for broadcasting

# `Broadcasted` wrap the arguments to `broadcast(f, args...)`. A statement like
#    y = x .* (x .+ 1)
# will result in code that is essentially
#    y = copy(Broadcasted(*, x, Broadcasted(+, x, 1)))
# `broadcast!` results in `copyto!(dest, Broadcasted(...))`.

# The use of `Nothing` in place of a `BroadcastStyle` has a different
# application, in the fallback method
#    copyto!(dest, bc::Broadcasted) = copyto!(dest, convert(Broadcasted{Nothing}, bc))
# This allows methods
#    copyto!(dest::DestType,  bc::Broadcasted{Nothing})
# that specialize on `DestType` to be easily disambiguated from
# methods that instead specialize on `BroadcastStyle`,
#    copyto!(dest::AbstractArray, bc::Broadcasted{MyStyle})

struct Broadcasted{Style<:Union{Nothing,BroadcastStyle}, Axes, F, Args<:Tuple} <: Base.AbstractBroadcasted
    f::F
    args::Args
    axes::Axes          # the axes of the resulting object (may be bigger than implied by `args` if this is nested inside a larger `Broadcasted`)
end

Broadcasted(f::F, args::Args, axes=nothing) where {F, Args<:Tuple} =
    Broadcasted{typeof(combine_styles(args...))}(f, args, axes)
function Broadcasted{Style}(f::F, args::Args, axes=nothing) where {Style, F, Args<:Tuple}
    # using Core.Typeof rather than F preserves inferrability when f is a type
    Broadcasted{Style, typeof(axes), Core.Typeof(f), Args}(f, args, axes)
end

struct AndAnd end
const andand = AndAnd()
broadcasted(::AndAnd, a, b) = broadcasted((a, b) -> a && b, a, b)
function broadcasted(::AndAnd, a, bc::Broadcasted)
    bcf = flatten(bc)
    broadcasted((a, args...) -> a && bcf.f(args...), a, bcf.args...)
end
struct OrOr end
const oror = OrOr()
broadcasted(::OrOr, a, b) = broadcasted((a, b) -> a || b, a, b)
function broadcasted(::OrOr, a, bc::Broadcasted)
    bcf = flatten(bc)
    broadcasted((a, args...) -> a || bcf.f(args...), a, bcf.args...)
end

Base.convert(::Type{Broadcasted{NewStyle}}, bc::Broadcasted{Style,Axes,F,Args}) where {NewStyle,Style,Axes,F,Args} =
    Broadcasted{NewStyle,Axes,F,Args}(bc.f, bc.args, bc.axes)

function Base.show(io::IO, bc::Broadcasted{Style}) where {Style}
    print(io, Broadcasted)
    # Only show the style parameter if we have a set of axes — representing an instantiated
    # "outermost" Broadcasted. The styles of nested Broadcasteds represent an intermediate
    # computation that is not relevant for dispatch, confusing, and just extra line noise.
    bc.axes isa Tuple && print(io, '{', Style, '}')
    print(io, '(', bc.f, ", ", bc.args, ')')
    nothing
end

## Allocating the output container
Base.similar(bc::Broadcasted, ::Type{T}) where {T} = similar(bc, T, axes(bc))
Base.similar(::Broadcasted{DefaultArrayStyle{N}}, ::Type{ElType}, dims) where {N,ElType} =
    similar(Array{ElType}, dims)
Base.similar(::Broadcasted{DefaultArrayStyle{N}}, ::Type{Bool}, dims) where {N} =
    similar(BitArray, dims)
# In cases of conflict we fall back on Array
Base.similar(::Broadcasted{ArrayConflict}, ::Type{ElType}, dims) where ElType =
    similar(Array{ElType}, dims)
Base.similar(::Broadcasted{ArrayConflict}, ::Type{Bool}, dims) =
    similar(BitArray, dims)

Base.axes(bc::Broadcasted) = _axes(bc, bc.axes)
_axes(::Broadcasted, axes::Tuple) = axes
_axes(bc::Broadcasted, ::Nothing)  = combine_axes(bc.args...)
_axes(bc::Broadcasted{<:AbstractArrayStyle{0}}, ::Nothing) = ()

Base.axes(bc::Broadcasted, d::Integer) = d <= ndims(bc) ? axes(bc)[d] : OneTo(1)

Base.axes1(bc::Broadcasted) = axes(bc, 1)
Base.axes1(bc::Broadcasted{<:AbstractArrayStyle{0}}) = OneTo(1)

BroadcastStyle(::Type{<:Broadcasted{Style}}) where {Style} = Style()
BroadcastStyle(::Type{<:Broadcasted{S}}) where {S<:Union{Nothing,Unknown}} =
    throw(ArgumentError("Broadcasted{Unknown} wrappers do not have a style assigned"))

argtype(::Type{Broadcasted{Style,Axes,F,Args}}) where {Style,Axes,F,Args} = Args
argtype(bc::Broadcasted) = argtype(typeof(bc))

Base.eachindex(bc::Broadcasted) = _eachindex(axes(bc))
_eachindex(t::Tuple{Any}) = t[1]
_eachindex(t::Tuple) = CartesianIndices(t)

Base.IndexStyle(bc::Broadcasted) = IndexStyle(typeof(bc))
Base.IndexStyle(::Type{<:Broadcasted{<:Any,<:Tuple{Any}}}) = IndexLinear()
Base.IndexStyle(::Type{<:Broadcasted{<:Any}}) = IndexCartesian()

Base.LinearIndices(bc::Broadcasted{<:Any,<:Tuple{Any}}) = LinearIndices(axes(bc))::LinearIndices{1}

Base.ndims(bc::Broadcasted) = ndims(typeof(bc))
Base.ndims(::Type{<:Broadcasted{<:Any,<:NTuple{N,Any}}}) where {N} = N
_fieldtypes(A) = ntuple(Base.Fix1(fieldtype,A), Val(fieldcount(A)))
Base.ndims(::Type{<:Broadcasted{<:Any,Nothing,<:Any,A}}) where {A<:Tuple} = mapfoldl(_ndims, max, _fieldtypes(A))
_ndims(x) = ndims(x)
_ndims(::Type{<:Tuple}) = 1

Base.size(bc::Broadcasted) = map(length, axes(bc))
Base.length(bc::Broadcasted) = prod(size(bc))

@propagate_inbounds function Base.iterate(bc::Broadcasted, s = (eachindex(bc),))
    y = iterate(s...)
    y === nothing && return nothing
    i, newstate = y
    return (bc[i], (s[1], newstate))
end

Base.IteratorSize(::Type{T}) where {T<:Broadcasted} = Base.HasShape{ndims(T)}()
Base.IteratorEltype(::Type{<:Broadcasted}) = Base.EltypeUnknown()

## Instantiation fills in the "missing" fields in Broadcasted.
instantiate(x) = x

"""
    Broadcast.instantiate(bc::Broadcasted)

Construct and check the axes for the lazy Broadcasted object `bc`.

Custom [`BroadcastStyle`](@ref)s may override this default in cases where it is fast and easy
to compute and verify the resulting `axes` on-demand, leaving the `axis` field
of the `Broadcasted` object empty (populated with [`nothing`](@ref)).
"""
function instantiate(bc::Broadcasted{Style}) where {Style}
    if bc.axes isa Nothing # Not done via dispatch to make it easier to extend instantiate(::Broadcasted{Style})
        axes = combine_axes(bc.args...)
    else
        axes = bc.axes
        check_broadcast_axes(axes, bc.args...)
    end
    return Broadcasted{Style}(bc.f, bc.args, axes)
end
instantiate(bc::Broadcasted{<:AbstractArrayStyle{0}}) = bc
# Tuples don't need axes, but when they have axes (for .= assignment), we need to check them (#33020)
instantiate(bc::Broadcasted{Style{Tuple}, Nothing}) = bc
function instantiate(bc::Broadcasted{Style{Tuple}})
    check_broadcast_axes(bc.axes, bc.args...)
    return bc
end
## Flattening

"""
    bcf = flatten(bc)

Create a "flat" representation of a lazy-broadcast operation.
From
   f.(a, g.(b, c), d)
we produce the equivalent of
   h.(a, b, c, d)
where
   h(w, x, y, z) = f(w, g(x, y), z)
In terms of its internal representation,
   Broadcasted(f, a, Broadcasted(g, b, c), d)
becomes
   Broadcasted(h, a, b, c, d)

This is an optional operation that may make custom implementation of broadcasting easier in
some cases.
"""
function flatten(bc::Broadcasted{Style}) where {Style}
    isflat(bc) && return bc
    # 1. concatenate the nested arguments into {a, b, c, d}
    # 2. build a function `makeargs` that takes a "flat" argument list and
    # and creates the appropriate input arguments for `f`, e.g.,
    #          makeargs = (w, x, y, z) -> (w, g(x, y), z)
    #
    # `makeargs` is built recursively and looks a bit like this:
    #     makeargs(w, x, y, z) = (w, makeargs1(x, y, z)...)
    #                          = (w, g(x, y), makeargs2(z)...)
    #                          = (w, g(x, y), z)
    headf, args = make_makeargs(bc.args, ())
    newf = RootNode(bc.f, headf)
    Broadcasted{Style}(newf, args, bc.axes)
end

const NestedTuple = Tuple{<:Broadcasted,Vararg{Any}}
isflat(bc::Broadcasted) = _isflat(bc.args)
_isflat(args::NestedTuple) = false
_isflat(args::Tuple) = _isflat(tail(args))
_isflat(args::Tuple{}) = true

"""
    make_makeargs(t::Tuple, args::Tuple) -> Function, Tuple

Each element of `t` is one (consecutive) node in a broadcast tree.
`args` contains the rest arguments on the "right" side of `t`.
The jobs of `make_makeargs` are:
    1. append the flattened arguments in `t` at the beginning of `args`.
    2. return a function that takes in flattened argument list and returns a
    tuple (each entry corresponding to an entry in `t`, having evaluated
    the corresponding element in the broadcast tree).
"""
make_makeargs(::Tuple{}, args) = tuple, args

function make_makeargs(t::Tuple, args)
    tailf, args′ = make_makeargs(tail(t), args)
    newf = tailf === tuple ? tuple : FlatNode(tailf) # avoid unneeded recursion
    newf, (t[1], args′...)
end

function make_makeargs(t::NestedTuple, args)
    bc = t[1]
    # Here we recurse into siblings in the broadcast tree.
    tailf, args′ = make_makeargs(tail(t), args)
    # Here we recurse into children.
    # It is more compiler frendly to treat `bc` as a new parent "node".
    headf, argsˢ = make_makeargs(bc.args, ())
    NestedNode{length(argsˢ)}(bc.f, headf, tailf), (argsˢ..., args′...)
end

# Some help structs to flatten `Broadcasted`.
# TODO: make them better printed in REPL.
struct RootNode{F,H} <: Function
    f::F
    prepare::H
end
RootNode(::Type{F}, prepare::H) where {F,H} = RootNode{Type{F},H}(F, prepare)
@inline (f::RootNode)(args::Vararg{Any}) = f.f(f.prepare(args...)...)

struct FlatNode{T} <: Function
    rest::T
end
@inline (f::FlatNode)(x, args::Vararg{Any}) = (x, f.rest(args...)...)

struct NestedNode{L,F,H,T} <: Function
    f::F
    prepare::H
    rest::T
end
NestedNode{L}(f::F, prepare::H, rest::T) where {L,F,T,H} = NestedNode{L,F,H,T}(f, prepare, rest)
NestedNode{L}(::Type{F}, prepare::H, rest::T) where {L,F,T,H} = NestedNode{L,Type{F},H,T}(F, prepare, rest)

# Specialize small `L` manually.
@inline (f::NestedNode{1})(x, args::Vararg{Any}) = (f.f(f.prepare(x)...), f.rest(args...)...)
@inline (f::NestedNode{2})(x1, x2, args::Vararg{Any}) = (f.f(f.prepare(x1, x2)...), f.rest(args...)...)
@inline (f::NestedNode{3})(x1, x2, x3, args::Vararg{Any}) = (f.f(f.prepare(x1, x2, x3)...), f.rest(args...)...)
@inline (f::NestedNode{4})(x1, x2, x3, x4, args::Vararg{Any}) = (f.f(f.prepare(x1, x2, x3, x4)...), f.rest(args...)...)
# Split based fallback.
@inline function (f::NestedNode{L})(args::Vararg{Any}) where {L}
    head, tail = Base.IteratorsMD.split(args, Val(L))
    (f.f(f.prepare(head...)...), f.rest(tail...)...)
end

Base.print(io::IO, ::RootNode) = Base.print(io, "flattened(::Vararg{Any})")

## Broadcasting utilities ##

## logic for deciding the BroadcastStyle

"""
    combine_styles(cs...) -> BroadcastStyle

Decides which `BroadcastStyle` to use for any number of value arguments.
Uses [`BroadcastStyle`](@ref) to get the style for each argument, and uses
[`result_style`](@ref) to combine styles.

# Examples

```jldoctest
julia> Broadcast.combine_styles([1], [1 2; 3 4])
Base.Broadcast.DefaultArrayStyle{2}()
```
"""
function combine_styles end

combine_styles() = DefaultArrayStyle{0}()
combine_styles(c) = result_style(BroadcastStyle(typeof(c)))
combine_styles(c1, c2) = result_style(combine_styles(c1), combine_styles(c2))
combine_styles(c1, c2, cs::Vararg{Any,N}) where {N} = result_style(combine_styles(c1), combine_styles(c2, cs...))

"""
    result_style(s1::BroadcastStyle[, s2::BroadcastStyle]) -> BroadcastStyle

Takes one or two `BroadcastStyle`s and combines them using [`BroadcastStyle`](@ref) to
determine a common `BroadcastStyle`.

# Examples

```jldoctest
julia> Broadcast.result_style(Broadcast.DefaultArrayStyle{0}(), Broadcast.DefaultArrayStyle{3}())
Base.Broadcast.DefaultArrayStyle{3}()

julia> Broadcast.result_style(Broadcast.Unknown(), Broadcast.DefaultArrayStyle{1}())
Base.Broadcast.DefaultArrayStyle{1}()
```
"""
function result_style end

result_style(s::BroadcastStyle) = s
result_style(::S, ::S) where S<:BroadcastStyle = S()
# Test both orders so users typically only have to declare one order
result_style(s1, s2) = result_join(s1, s2, BroadcastStyle(s1, s2), BroadcastStyle(s2, s1))

# result_join is the final arbiter. Because `BroadcastStyle` for undeclared pairs results in Unknown,
# we defer to any case where the result of `BroadcastStyle` is known.
result_join(::Any, ::Any, ::Unknown, ::Unknown)   = Unknown()
result_join(::Any, ::Any, ::Unknown, s::BroadcastStyle) = s
result_join(::Any, ::Any, s::BroadcastStyle, ::Unknown) = s
# For AbstractArray types with specialized broadcasting and undefined precedence rules,
# we have to signal conflict. Because ArrayConflict is a subtype of AbstractArray,
# this will "poison" any future operations (if we instead returned `DefaultArrayStyle`, then for
# 3-array broadcasting the returned type would depend on argument order).
result_join(::AbstractArrayStyle, ::AbstractArrayStyle, ::Unknown, ::Unknown) =
    ArrayConflict()
# Fallbacks in case users define `rule` for both argument-orders (not recommended)
result_join(::Any, ::Any, ::S, ::S) where S<:BroadcastStyle = S()
@noinline function result_join(::S, ::T, ::U, ::V) where {S,T,U,V}
    error("""
conflicting broadcast rules defined
  Broadcast.BroadcastStyle(::$S, ::$T) = $U()
  Broadcast.BroadcastStyle(::$T, ::$S) = $V()
One of these should be undefined (and thus return Broadcast.Unknown).""")
end

# Indices utilities

"""
    combine_axes(As...) -> Tuple

Determine the result axes for broadcasting across all values in `As`.

```jldoctest
julia> Broadcast.combine_axes([1], [1 2; 3 4; 5 6])
(Base.OneTo(3), Base.OneTo(2))

julia> Broadcast.combine_axes(1, 1, 1)
()
```
"""
combine_axes(A, B::Vararg{Any,N}) where {N} = broadcast_shape(axes(A), combine_axes(B...))
combine_axes(A, B) = broadcast_shape(axes(A), axes(B))
combine_axes(A) = axes(A)

# shape (i.e., tuple-of-indices) inputs
broadcast_shape(shape::Tuple) = shape
broadcast_shape(shape::Tuple, shape1::Tuple, shapes::Vararg{Tuple,N}) where {N} = broadcast_shape(_bcs(shape, shape1), shapes...)
# _bcs consolidates two shapes into a single output shape
_bcs(::Tuple{}, ::Tuple{}) = ()
_bcs(::Tuple{}, newshape::Tuple) = newshape
_bcs(shape::Tuple, ::Tuple{}) = shape
_bcs(shape::Tuple, newshape::Tuple) =
    (_bcs1(shape[1], newshape[1]), _bcs(tail(shape), tail(newshape))...)
# _bcs1 handles the logic for a single dimension
_bcs1(a::Integer, b::Integer) = a == 1 ? b : (b == 1 ? a : (a == b ? a : throw(DimensionMismatch("arrays could not be broadcast to a common size; got a dimension with lengths $a and $b"))))
_bcs1(a::Integer, b) = a == 1 ? b : (first(b) == 1 && last(b) == a ? b : throw(DimensionMismatch("arrays could not be broadcast to a common size; got a dimension with lengths $a and $(length(b))")))
_bcs1(a, b::Integer) = _bcs1(b, a)
_bcs1(a, b) = _bcsm(b, a) ? axistype(b, a) : (_bcsm(a, b) ? axistype(a, b) : throw(DimensionMismatch("arrays could not be broadcast to a common size; got a dimension with lengths $(length(a)) and $(length(b))")))
# _bcsm tests whether the second index is consistent with the first
_bcsm(a, b) = a == b || length(b) == 1
_bcsm(a, b::Number) = b == 1
_bcsm(a::Number, b::Number) = a == b || b == 1
# Ensure inferrability when dealing with axes of different AbstractUnitRange types
# (We may not want to define general promotion rules between, say, OneTo and Slice, but if
#  we get here we know the axes are at least consistent for the purposes of broadcasting)
axistype(a::T, b::T) where T = a
axistype(a::OneTo, b::OneTo) = OneTo{Int}(a)
axistype(a, b) = UnitRange{Int}(a)

## Check that all arguments are broadcast compatible with shape
# comparing one input against a shape
check_broadcast_shape(shp) = nothing
check_broadcast_shape(shp, ::Tuple{}) = nothing
check_broadcast_shape(::Tuple{}, ::Tuple{}) = nothing
function check_broadcast_shape(::Tuple{}, Ashp::Tuple)
    if any(ax -> length(ax) != 1, Ashp)
        throw(DimensionMismatch("cannot broadcast array to have fewer non-singleton dimensions"))
    end
    nothing
end
function check_broadcast_shape(shp, Ashp::Tuple)
    _bcsm(shp[1], Ashp[1]) || throw(DimensionMismatch("array could not be broadcast to match destination"))
    check_broadcast_shape(tail(shp), tail(Ashp))
end
check_broadcast_axes(shp, A) = check_broadcast_shape(shp, axes(A))
# comparing many inputs
function check_broadcast_axes(shp, A, As::Vararg{Any,N}) where {N}
    check_broadcast_axes(shp, A)
    check_broadcast_axes(shp, As...)
end

## Indexing manipulations
"""
    newindex(argument, I)
    newindex(I, keep, default)

Recompute index `I` such that it appropriately constrains broadcasted dimensions to the source.

Two methods are supported, both allowing for `I` to be specified as either a [`CartesianIndex`](@ref) or
an `Int`.

* `newindex(argument, I)` dynamically constrains `I` based upon the axes of `argument`.
* `newindex(I, keep, default)` constrains `I` using the pre-computed tuples `keeps` and `defaults`.
    * `keep` is a tuple of `Bool`s, where `keep[d] == true` means that dimension `d` in `I` should be preserved as is
    * `default` is a tuple of Integers, specifying what index to use in dimension `d` when `keep[d] == false`.
    Any remaining indices in `I` beyond the length of the `keep` tuple are truncated. The `keep` and `default`
    tuples may be created by `newindexer(argument)`.
"""
@propagate_inbounds newindex(arg, I::CartesianIndex) = CartesianIndex(_newindex(axes(arg), I.I))
@propagate_inbounds newindex(arg, I::Integer) = CartesianIndex(_newindex(axes(arg), (I,)))
@propagate_inbounds _newindex(ax::Tuple, I::Tuple) = (ifelse(length(ax[1]) == 1, ax[1][1], I[1]), _newindex(tail(ax), tail(I))...)
@propagate_inbounds _newindex(ax::Tuple{}, I::Tuple) = ()
@propagate_inbounds _newindex(ax::Tuple, I::Tuple{}) = (ax[1][1], _newindex(tail(ax), ())...)
@propagate_inbounds _newindex(ax::Tuple{}, I::Tuple{}) = ()

# If dot-broadcasting were already defined, this would be `ifelse.(keep, I, Idefault)`.
newindex(::Integer, ::Tuple{}, ::Tuple{}) = CartesianIndex()
newindex(i::Integer, keep::Tuple{Bool}, default::Tuple{Integer}) = ifelse(keep[1], i, oftype(i, default[1]))
newindex(i::Integer, keep::NTuple{N,Bool}, default::NTuple{N,Integer}) where {N} =
    CartesianIndex(ifelse(keep[1], Int(i), Int(default[1])), default[2])
newindex(I::CartesianIndex{1}, keep, default) = newindex(I.I[1], keep, default)
newindex(I::CartesianIndex, keep, default) = CartesianIndex(_newindex(I.I, keep, default))
_newindex(I::Tuple{Int,Vararg{Int}}, keep::NTuple{N,Bool}, default::NTuple{N,Integer}) where {N} =
    (ifelse(keep[1], I[1], Int(default[1])), _newindex(tail(I), tail(keep), tail(default))...)
_newindex(::Tuple{Int,Vararg{Int}}, ::Tuple{}, ::Tuple{}) = ()  # truncate if keep is shorter than I
_newindex(::Tuple{}, ::NTuple{N,Bool}, ::NTuple{N,Integer}) where {N} = () # and if I is shorter than keep

# newindexer(A) generates `keep` and `Idefault` (for use by `newindex` above)
# for a particular array `A`; `shapeindexer` does so for its axes.
newindexer(A) = shapeindexer(axes(A))
shapeindexer(ax) = _newindexer(ax)
_newindexer(indsA::Tuple{}) = (), ()
function _newindexer(indsA::Tuple)
    ind1 = indsA[1]
    keep, Idefault = _newindexer(tail(indsA))
    (Base.length(ind1)::Integer != 1, keep...), (first(ind1), Idefault...)
end

@inline function Base.getindex(bc::Broadcasted, I::Union{Integer,CartesianIndex})
    @boundscheck checkbounds(bc, I)
    @inbounds _broadcast_getindex(bc, I)
end
@propagate_inbounds Base.getindex(
    bc::Broadcasted,
    i1::Union{Integer,CartesianIndex},
    i2::Union{Integer,CartesianIndex},
    I::Vararg{Union{Integer,CartesianIndex},N}
) where {N} =
    bc[CartesianIndex((i1, i2, I...))]
@propagate_inbounds Base.getindex(bc::Broadcasted) = bc[CartesianIndex()]

Base.checkbounds(bc::Broadcasted, I::Union{Integer,CartesianIndex}) =
    Base.checkbounds_indices(Bool, axes(bc), (I,)) || Base.throw_boundserror(bc, (I,))


"""
    _broadcast_getindex(A, I)

Index into `A` with `I`, collapsing broadcasted indices to their singleton indices as appropriate.
"""
@propagate_inbounds _broadcast_getindex(A::Union{Ref,Some,AbstractArray{<:Any,0},Number}, I) = A[] # Scalar-likes can just ignore all indices
RefSome{T} = Union{Ref{T},Some{T}}
@propagate_inbounds _broadcast_getindex(::RefSome{Type{T}}, I) where {T} = T
# Tuples are statically known to be singleton or vector-like
@propagate_inbounds _broadcast_getindex(A::Tuple{Any}, I) = A[1]
@propagate_inbounds _broadcast_getindex(A::Tuple, I) = A[I[1]]
# Everything else falls back to dynamically dropping broadcasted indices based upon its axes
@propagate_inbounds _broadcast_getindex(A, I) = A[newindex(A, I)]

# In some cases, it's more efficient to sort out which dimensions should be dropped
# ahead of time (often when the size checks aren't able to be lifted out of the loop).
# The Extruded struct computes that information ahead of time and stores it as a pair
# of tuples to optimize indexing later. This is most commonly needed for `Array` and
# other `AbstractArray` subtypes that wrap `Array` and dynamically ask it for its size.
struct Extruded{N,K,D,T}
    x::T
    keeps::NTuple{K,Bool}   # A tuple of booleans, specifying which indices should be passed normally
    defaults::D # A tuple of integers, specifying the index to use when keeps[i] is false (as defaults[i])
    Extruded(x::T, keeps::NTuple{K,Bool}, defaults::NTuple{D,Integer}) where {T,K,D} =
        new{ndims(T),K,typeof(defaults),T}(x,keeps,defaults)
end
axes(b::Extruded) = axes(b.x)
StandardExtruded{N} = Extruded{N,N,<:NTuple{N,Integer}}
@propagate_inbounds _broadcast_getindex(b::StandardExtruded, i::Union{CartesianIndex{1},Integer}) = b.x[newindex(i, b.keeps, b.defaults)]
@propagate_inbounds _broadcast_getindex(b::StandardExtruded, i::CartesianIndex) = b.x[newindex(i, b.keeps, b.defaults)]
KeepDim1Extruded{N,M} = Extruded{M,N,<:NTuple{N,Integer}}
@propagate_inbounds _broadcast_getindex(b::KeepDim1Extruded{0}, i::Union{CartesianIndex{1},Integer}) = b.x[i]
@propagate_inbounds _broadcast_getindex(b::KeepDim1Extruded, i::Union{CartesianIndex{1},Integer}) = b.x[i,b.defaults[1]]
@propagate_inbounds _broadcast_getindex(b::KeepDim1Extruded, i::CartesianIndex) =
    b.x[i.I[1], newindex(CartesianIndex(tail(i.I)), b.keeps, b.defaults)]
DropDim1Extruded{N,M} = Extruded{N,M,<:NTuple{N,Integer}}
@propagate_inbounds _broadcast_getindex(b::DropDim1Extruded, i::Union{CartesianIndex{1},Integer}) = b.x[]
@propagate_inbounds _broadcast_getindex(b::DropDim1Extruded, i::CartesianIndex) =
    b.x[b.defaults[1], newindex(CartesianIndex(tail(i.I)), b.keeps, tail(b.defaults))]

# Most general extrude, ndims(x) == length(keeps) == length(defaults)
extrude(x::AbstractArray) = ndims(x) == 0 ? x : Extruded(x, newindexer(x)...)
extrude(x) = x
# Always preserve the 1st dimension, ndims(x) == length(keeps) + 1 == length(defaults) + 1
knownsize1(::Any) = false
knownsize1(::Union{Ref,Base.AbstractZeroDimArray,Number,Tuple}) = true
extrudeskipdim1(x::AbstractArray) = knownsize1(x) ? extrude(x) : Extruded(x, map(tail, newindexer(x))...)
extrudeskipdim1(x) = x
# For dynamic dispatch:
# * If we need to preserve the 1st dimension ndims(x) == length(keeps) + 1 == length(defaults) + 1.
# * Otherwise, ndims(x) == length(keeps) + 1 == length(defaults).
function extrudeconstdim1(x::AbstractArray)
    knownsize1(x) && return extrude(x)
    k, d = newindexer(x)
    Extruded(x, tail(k), k[1] ? tail(d) : d)
end
extrudeconstdim1(x) = x

# For Broadcasted
@propagate_inbounds function _broadcast_getindex(bc::Broadcasted, I)
    args = _getindex(bc.args, I)
    return _broadcast_getindex_evalf(bc.f, args...)
end

# Utilities for _broadcast_getindex
@propagate_inbounds _getindex(args::Tuple, I) = (_broadcast_getindex(args[1], I), _getindex(tail(args), I)...)
@propagate_inbounds _getindex(args::Tuple{}, I) = ()

_broadcast_getindex_evalf(f::Tf, args::Vararg{Any,N}) where {Tf,N} = f(args...)  # not propagate_inbounds

"""
    Broadcast.broadcastable(x)

Return either `x` or an object like `x` such that it supports [`axes`](@ref), indexing, and its type supports [`ndims`](@ref).

If `x` supports iteration, the returned value should have the same `axes` and indexing
behaviors as [`collect(x)`](@ref).

If `x` is not an `AbstractArray` but it supports `axes`, indexing, and its type supports
`ndims`, then `broadcastable(::typeof(x))` may be implemented to just return itself.
Further, if `x` defines its own [`BroadcastStyle`](@ref), then it must define its
`broadcastable` method to return itself for the custom style to have any effect.

# Examples
```jldoctest
julia> Broadcast.broadcastable([1,2,3]) # like `identity` since arrays already support axes and indexing
3-element Vector{Int64}:
 1
 2
 3

julia> Broadcast.broadcastable(Int) # Types don't support axes, indexing, or iteration but are commonly used as scalars
Base.Some{Type{Int64}}(Int64)

julia> Broadcast.broadcastable("hello") # Strings break convention of matching iteration and act like a scalar instead
Base.Some{String}("hello")
```
"""
broadcastable(x::Union{Symbol,AbstractString,Function,UndefInitializer,Nothing,RoundingMode,Missing,Val,Ptr,AbstractPattern,Pair,IO}) = Some(x)
broadcastable(::Type{T}) where {T} = Some{Type{T}}(T)
broadcastable(x::Union{AbstractArray,Number,AbstractChar,Some,Ref,Tuple,Broadcasted}) = x
# Default to collecting iterables — which will error for non-iterables
broadcastable(x) = collect(x)
broadcastable(::Union{AbstractDict, NamedTuple}) = throw(ArgumentError("broadcasting over dictionaries and `NamedTuple`s is reserved"))

## Computation of inferred result type, for empty and concretely inferred cases only
_broadcast_getindex_eltype(bc::Broadcasted) = Base._return_type(_broadcast_getindex, Tuple{typeof(bc),eltype(eachindex(bc))})
_broadcast_getindex_eltype(A) = eltype(A)
# Inferred eltype of result of broadcast(f, args...)
combine_eltypes(f, args::Tuple) =
    promote_typejoin_union(Base._return_type(f, Tuple{map(_broadcast_getindex_eltype, args)...}))
## Broadcasting core

"""
    broadcast(f, As...)

Broadcast the function `f` over the arrays, tuples, collections, [`Some`](@ref)s and/or scalars `As`.

Broadcasting applies the function `f` over the elements of the container arguments and the
scalars themselves in `As`. Singleton and missing dimensions are expanded to match the
extents of the other arguments by virtually repeating the value. By default, only a limited
number of types are considered scalars, including `Number`s, `String`s, `Symbol`s, `Type`s,
`Function`s and some common singletons like [`missing`](@ref) and [`nothing`](@ref). All other arguments are
iterated over or indexed into elementwise.

The resulting container type is established by the following rules:

 - If all the arguments are scalars or zero-dimensional arrays, it returns an unwrapped scalar.
 - If at least one argument is a tuple and all others are scalars or zero-dimensional arrays,
   it returns a tuple.
 - All other combinations of arguments default to returning an `Array`, but
   custom container types can define their own implementation and promotion-like
   rules to customize the result when they appear as arguments.

A special syntax exists for broadcasting: `f.(args...)` is equivalent to
`broadcast(f, args...)`, and nested `f.(g.(args...))` calls are fused into a
single broadcast loop.

# Examples
```jldoctest
julia> A = [1, 2, 3, 4, 5]
5-element Vector{Int64}:
 1
 2
 3
 4
 5

julia> B = [1 2; 3 4; 5 6; 7 8; 9 10]
5×2 Matrix{Int64}:
 1   2
 3   4
 5   6
 7   8
 9  10

julia> broadcast(+, A, B)
5×2 Matrix{Int64}:
  2   3
  5   6
  8   9
 11  12
 14  15

julia> parse.(Int, ["1", "2"])
2-element Vector{Int64}:
 1
 2

julia> abs.((1, -2))
(1, 2)

julia> broadcast(+, 1.0, (0, -2.0))
(1.0, -1.0)

julia> (+).([[0,2], [1,3]], Some{Vector{Int}}([1,-1]))
2-element Vector{Vector{Int64}}:
 [1, 1]
 [2, 2]

julia> string.(("one","two","three","four"), ": ", 1:4)
4-element Vector{String}:
 "one: 1"
 "two: 2"
 "three: 3"
 "four: 4"

```
"""
broadcast(f::Tf, As::Vararg{Any,N}) where {Tf,N} = materialize(broadcasted(f, As...))

# special cases defined for performance
broadcast(f, x::Number...) = f(x...)
broadcast(f, t::NTuple{N,Any}, ts::Vararg{NTuple{N,Any}}) where {N} = map(f, t, ts...)

"""
    broadcast!(f, dest, As...)

Like [`broadcast`](@ref), but store the result of
`broadcast(f, As...)` in the `dest` array.
Note that `dest` is only used to store the result, and does not supply
arguments to `f` unless it is also listed in the `As`,
as in `broadcast!(f, A, A, B)` to perform `A[:] = broadcast(f, A, B)`.

# Examples
```jldoctest
julia> A = [1.0; 0.0]; B = [0.0; 0.0];

julia> broadcast!(+, B, A, (0, -2.0));

julia> B
2-element Vector{Float64}:
  1.0
 -2.0

julia> A
2-element Vector{Float64}:
 1.0
 0.0

julia> broadcast!(+, A, A, (0, -2.0));

julia> A
2-element Vector{Float64}:
  1.0
 -2.0
```
"""
broadcast!(f::Tf, dest, As::Vararg{Any,N}) where {Tf,N} = (materialize!(dest, broadcasted(f, As...)); dest)

"""
    broadcast_preserving_zero_d(f, As...)

Like [`broadcast`](@ref), except in the case of a 0-dimensional result where it returns a 0-dimensional container

Broadcast automatically unwraps zero-dimensional results to be just the element itself,
but in some cases it is necessary to always return a container — even in the 0-dimensional case.
"""
function broadcast_preserving_zero_d(f, As...)
    bc = broadcasted(f, As...)
    r = materialize(bc)
    return length(axes(bc)) == 0 ? fill!(similar(bc, typeof(r)), r) : r
end
broadcast_preserving_zero_d(f) = fill(f())
broadcast_preserving_zero_d(f, as::Number...) = fill(f(as...))

"""
    Broadcast.materialize(bc)

Take a lazy `Broadcasted` object and compute the result
"""
materialize(bc::Broadcasted) = copy(instantiate(bc))
materialize(x) = x

function materialize!(dest, x)
    return materialize!(dest, instantiate(Broadcasted(identity, (x,), axes(dest))))
end

function materialize!(dest, bc::Broadcasted{Style}) where {Style}
    return materialize!(combine_styles(dest, bc), dest, bc)
end
function materialize!(::BroadcastStyle, dest, bc::Broadcasted{Style}) where {Style}
    return copyto!(dest, instantiate(Broadcasted{Style}(bc.f, bc.args, axes(dest))))
end

## general `copy` methods
copy(bc::Broadcasted{<:AbstractArrayStyle{0}}) = bc[CartesianIndex()]
copy(bc::Broadcasted{<:Union{Nothing,Unknown}}) =
    throw(ArgumentError("broadcasting requires an assigned BroadcastStyle"))

const NonleafHandlingStyles = Union{DefaultArrayStyle,ArrayConflict}

function copy(bc::Broadcasted{Style}) where {Style}
    ElType = promote_typejoin_union(_broadcast_getindex_eltype(bc))
    if isconcretetype(ElType)
        # We can trust it and defer to the simpler `copyto!`
        return copyto!(similar(bc, ElType), bc)
    end
    # When ElType is not concrete, use narrowing. Use the first output
    # value to determine the starting output eltype; copyto_nonleaf!
    # will widen `dest` as needed to accommodate later values.
    bc′ = preprocess(nothing, bc)
    iter = eachindex(bc′)
    y = iterate(iter)
    if y === nothing
        # if empty, take the ElType at face value
        return similar(bc′, ElType)
    end
    # Initialize using the first value
    I, state = y
    @inbounds val = bc′[I]
    dest = similar(bc′, typeof(val))
    @inbounds dest[I] = val
    # Now handle the remaining values
    # The typeassert gives inference a helping hand on the element type and dimensionality
    # (work-around for #28382)
    ElType′ = ElType === Union{} ? Any : ElType <: Type ? Type : ElType
    RT = dest isa AbstractArray ? AbstractArray{<:ElType′, ndims(dest)} : Any
    return copyto_nonleaf!(dest, bc′, iter, state, 1)::RT
end

## general `copyto!` methods
# The most general method falls back to a method that replaces Style->Nothing
# This permits specialization on typeof(dest) without introducing ambiguities
copyto!(dest::AbstractArray, bc::Broadcasted) = copyto!(dest, convert(Broadcasted{Nothing}, bc))

# Performance optimization for the common identity scalar case: dest .= val
function copyto!(dest::AbstractArray, bc::Broadcasted{<:AbstractArrayStyle{0}})
    # Typically, we must independently execute bc for every storage location in `dest`, but:
    # IF we're in the common no-op identity case with no nested args (like `dest .= val`),
    if bc.f === identity && bc.args isa Tuple{Any} && isflat(bc)
        # THEN we can just extract the argument and `fill!` the destination with it
        return fill!(dest, bc.args[1][])
    else
        # Otherwise, fall back to the default implementation like above
        return copyto!(dest, convert(Broadcasted{Nothing}, bc))
    end
end

# For broadcasted assignments like `broadcast!(f, A, ..., A, ...)`, where `A`
# appears on both the LHS and the RHS of the `.=`, then we know we're only
# going to make one pass through the array, and even though `A` is aliasing
# against itself, the mutations won't affect the result as the indices on the
# LHS and RHS will always match. This is not true in general, but with the `.op=`
# syntax it's fairly common for an argument to be `===` a source.
broadcast_unalias(dest, src) = dest === src ? src : unalias(dest, src)
broadcast_unalias(::Nothing, src) = src

# Preprocessing a `Broadcasted` does three things:
# * unaliases any arguments from `dest`
# * checks whether src arrays' `getindex` have side-effect
# * "extrudes" the arguments where it is advantageous to pre-compute the broadcasted indices
preprocess(dest, x) = @inline preprocess(extrude, dest, x)
preprocess(f::Op, dest, bc::Broadcasted{Style}) where {Op,Style} = Broadcasted{Style}(bc.f, preprocess_args(f, dest, bc.args), bc.axes)
preprocess(f::Op, dest, x) where {Op} = f(broadcast_unalias(dest, x))
@inline preprocess_args(f::Op, dest, args::Tuple) where {Op} = (preprocess(f, dest, args[1]), preprocess_args(f, dest, tail(args))...)
preprocess_args(f, dest, args::Tuple{}) = ()

isivdepsafe(dest::AbstractArray, bc::Broadcasted) = isivdepsafe(bc) && isivdepsafe(dest)
# Make sure bc[I] is effect_free
function isivdepsafe(bc::Broadcasted)
    eff = Core.Compiler.infer_effects(_broadcast_getindex, Tuple{typeof(bc),eltype(eachindex(bc))})
    eff.effect_free === Core.Compiler.ALWAYS_TRUE
end
# Make sure `dest` is not self-aliased
isivdepsafe(x::AbstractArray) = false
isivdepsafe(x::DenseArray) = true
isivdepsafe(x::Union{Base.ReshapedArray,Base.ReinterpretArray}) = isivdepsafe(parent(x))
isivdepsafe(x::Base.SubArray) = isivdepsafe(parent(x)) && all(_allunique, x.indices)
# Pure type-based check could avoid binary bloat.
_allunique(x) = allunique(x)
_allunique(::Number) = true
_allunique(::AbstractArray{<:Any,0}) = true
_allunique(::AbstractUnitRange) = true
_allunique(::StepRange) = true # step(::StepRange) should not be zero

# Specialize this method if all you want to do is specialize on typeof(dest)
function copyto!(dest::AbstractArray, bc::Broadcasted{Nothing})
    axes(dest) == axes(bc) || throwdm(axes(dest), axes(bc))
    # Performance optimization: broadcast!(identity, dest, A) is equivalent to copyto!(dest, A) if indices match
    if bc.f === identity && bc.args isa Tuple{AbstractArray} # only a single input argument to broadcast!
        A = bc.args[1]
        if axes(dest) == axes(A)
            return copyto!(dest, A)
        end
    end
    if countunknown(bc.args) <= 3
        # LLVM optimizes "small" Broadcasted well
        @inline return copyto_might_ivdep!(dest, preprocess(dest, bc))
    elseif checkaxes1(Base.axes1(dest), bc.args)
        # If all unknown arguments have the same 1st axes, we'd better keep type-stability
        return copyto_might_ivdep!(dest, preprocess(extrudeskipdim1, dest, bc))
    elseif length(dest) < 2048
        # If the problem is too small, then we'd better keep type-stability
        return copyto_might_ivdep!(dest, preprocess(dest, bc))
    else
        # Otherwise use dynamic dispatch to speed up
        return copyto_might_ivdep!(dest, preprocess(extrudeconstdim1, dest, flatten(bc)))
    end
end

# A help wrapper whose parent's getindex should has no side-effect and thus could be parallelly loaded.
countunknown(x::Tuple{Broadcasted,Vararg{Any}}) = countunknown(x[1].args) + countunknown(tail(x))
countunknown(x::Tuple) = !knownsize1(x[1]) + countunknown(tail(x))
countunknown(::Tuple{}) = 0

checkaxes1(ax1, x::Tuple{Broadcasted,Vararg{Any}}) = checkaxes1(ax1, x[1].args) && checkaxes1(ax1, tail(x))
checkaxes1(ax1, x::Tuple) = (knownsize1(x[1]) || Base.axes1(x[1]) == ax1) && checkaxes1(ax1, tail(x))
checkaxes1(ax1, ::Tuple{}) = true

function copyto_might_ivdep!(dest::AbstractArray, bc::Broadcasted{Nothing})
    @inbounds if isivdepsafe(dest, bc)
        @simd ivdep for I in eachindex(bc)
            dest[I] = bc[I]
        end
    else
        @simd for I in eachindex(bc)
            dest[I] = bc[I]
        end
    end
    return dest
end

# Performance optimization: for BitArray outputs, we cache the result
# in a "small" Vector{Bool}, and then copy in chunks into the output
function copyto!(dest::BitArray, bc::Broadcasted{Nothing})
    axes(dest) == axes(bc) || throwdm(axes(dest), axes(bc))
    ischunkedbroadcast(dest, bc) && return chunkedcopyto!(dest, bc)
    length(dest) < 256 && return invoke(copyto!, Tuple{AbstractArray, Broadcasted{Nothing}}, dest, bc)
    tmp = Vector{Bool}(undef, bitcache_size)
    destc = dest.chunks
    cind = 1
    bc′ = preprocess(dest, bc)
    @inbounds for P in Iterators.partition(eachindex(bc′), bitcache_size)
        ind = 1
        @simd for I in P
            tmp[ind] = bc′[I]
            ind += 1
        end
        @simd for i in ind:bitcache_size
            tmp[i] = false
        end
        dumpbitcache(destc, cind, tmp)
        cind += bitcache_chunks
    end
    return dest
end

# For some BitArray operations, we can work at the level of chunks. The trivial
# implementation just walks over the UInt64 chunks in a linear fashion.
# This requires three things:
#   1. The function must be known to work at the level of chunks (or can be converted to do so)
#   2. The only arrays involved must be BitArrays or scalar Bools
#   3. There must not be any broadcasting beyond scalar — all array sizes must match
# We could eventually allow for all broadcasting and other array types, but that
# requires very careful consideration of all the edge effects.
const ChunkableOp = Union{typeof(&), typeof(|), typeof(xor), typeof(~), typeof(identity),
    typeof(!), typeof(*), typeof(==)} # these are convertible to chunkable ops by liftfuncs
const BroadcastedChunkableOp{Style<:Union{Nothing,BroadcastStyle}, Axes, F<:ChunkableOp, Args<:Tuple} = Broadcasted{Style,Axes,F,Args}
ischunkedbroadcast(R, bc::BroadcastedChunkableOp) = ischunkedbroadcast(R, bc.args)
ischunkedbroadcast(R, args) = false
ischunkedbroadcast(R, args::Tuple{<:BitArray,Vararg{Any}}) = size(R) == size(args[1]) && ischunkedbroadcast(R, tail(args))
ischunkedbroadcast(R, args::Tuple{<:Bool,Vararg{Any}}) = ischunkedbroadcast(R, tail(args))
ischunkedbroadcast(R, args::Tuple{<:BroadcastedChunkableOp,Vararg{Any}}) = ischunkedbroadcast(R, args[1]) && ischunkedbroadcast(R, tail(args))
ischunkedbroadcast(R, args::Tuple{}) = true

# Convert compatible functions to chunkable ones. They must also be green-lighted as ChunkableOps
liftfuncs(bc::Broadcasted{Style}) where {Style} = Broadcasted{Style}(bc.f, map(liftfuncs, bc.args), bc.axes)
liftfuncs(bc::Broadcasted{Style,<:Any,typeof(sign)}) where {Style} = Broadcasted{Style}(identity, map(liftfuncs, bc.args), bc.axes)
liftfuncs(bc::Broadcasted{Style,<:Any,typeof(!)}) where {Style} = Broadcasted{Style}(~, map(liftfuncs, bc.args), bc.axes)
liftfuncs(bc::Broadcasted{Style,<:Any,typeof(*)}) where {Style} = Broadcasted{Style}(&, map(liftfuncs, bc.args), bc.axes)
liftfuncs(bc::Broadcasted{Style,<:Any,typeof(==)}) where {Style} = Broadcasted{Style}((~)∘(xor), map(liftfuncs, bc.args), bc.axes)
liftfuncs(x) = x

liftchunks(::Tuple{}) = ()
liftchunks(args::Tuple{<:BitArray,Vararg{Any}}) = (args[1].chunks, liftchunks(tail(args))...)
# Transform scalars to repeated scalars the size of a chunk
liftchunks(args::Tuple{<:Bool,Vararg{Any}}) = (ifelse(args[1], typemax(UInt64), UInt64(0)), liftchunks(tail(args))...)
ithchunk(i) = ()
@propagate_inbounds ithchunk(i, c::Vector{UInt64}, args::Vararg{Any,N}) where {N} = (c[i], ithchunk(i, args...)...)
@propagate_inbounds ithchunk(i, b::UInt64, args::Vararg{Any,N}) where {N} = (b, ithchunk(i, args...)...)
function chunkedcopyto!(dest::BitArray, bc::Broadcasted)
    isempty(dest) && return dest
    f = flatten(liftfuncs(bc))
    args = liftchunks(f.args)
    dc = dest.chunks
    @simd for i in eachindex(dc)
        @inbounds dc[i] = f.f(ithchunk(i, args...)...)
    end
    @inbounds dc[end] &= Base._msk_end(dest)
    return dest
end


@noinline throwdm(axdest, axsrc) =
    throw(DimensionMismatch("destination axes $axdest are not compatible with source axes $axsrc"))

function restart_copyto_nonleaf!(newdest, dest, bc, val, I, iter, state, count)
    # Function barrier that makes the copying to newdest type stable
    for II in Iterators.take(iter, count)
        newdest[II] = dest[II]
    end
    newdest[I] = val
    return copyto_nonleaf!(newdest, bc, iter, state, count+1)
end

function copyto_nonleaf!(dest, bc::Broadcasted, iter, state, count)
    T = eltype(dest)
    while true
        y = iterate(iter, state)
        y === nothing && break
        I, state = y
        @inbounds val = bc[I]
        if val isa T || typeof(val) === T
            @inbounds dest[I] = val
        else
            # This element type doesn't fit in dest. Allocate a new dest with wider eltype,
            # copy over old values, and continue
            newdest = Base.similar(bc, promote_typejoin(T, typeof(val)))
            return restart_copyto_nonleaf!(newdest, dest, bc, val, I, iter, state, count)
        end
        count += 1
    end
    return dest
end

## Tuple methods

function copy(bc::Broadcasted{Style{Tuple}})
    dim = axes(bc)
    length(dim) == 1 || throw(DimensionMismatch("tuple only supports one dimension"))
    N = length(dim[1])
    return ntuple(k -> @inbounds(_broadcast_getindex(bc, k)), Val(N))
end

## scalar-range broadcast operations ##
# DefaultArrayStyle and \ are not available at the time of range.jl
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::AbstractRange) = r

broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::AbstractRange) = range(-first(r), step=negate(step(r)), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::OrdinalRange) = range(-first(r), -last(r), step=negate(step(r)))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::StepRangeLen) = StepRangeLen(-r.ref, negate(r.step), length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::LinRange) = LinRange(-r.start, -r.stop, length(r))

# For #18336 we need to prevent promotion of the step type:
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::AbstractRange, x::Number) = range(first(r) + x, step=step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Number, r::AbstractRange) = range(x + first(r), step=step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::OrdinalRange, x::Integer) = range(first(r) + x, last(r) + x, step=step(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Integer, r::OrdinalRange) = range(x + first(r), x + last(r), step=step(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::AbstractUnitRange, x::Integer) = range(first(r) + x, last(r) + x)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Integer, r::AbstractUnitRange) = range(x + first(r), x + last(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::AbstractUnitRange, x::Real) = range(first(r) + x, length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Real, r::AbstractUnitRange) = range(x + first(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::StepRangeLen{T}, x::Number) where T =
    StepRangeLen{typeof(T(r.ref)+x)}(r.ref + x, r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Number, r::StepRangeLen{T}) where T =
    StepRangeLen{typeof(x+T(r.ref))}(x + r.ref, r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r::LinRange, x::Number) = LinRange(r.start + x, r.stop + x, length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), x::Number, r::LinRange) = LinRange(x + r.start, x + r.stop, length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(+), r1::AbstractRange, r2::AbstractRange) = r1 + r2

broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::AbstractRange, x::Number) = range(first(r) - x, step=step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), x::Number, r::AbstractRange) = range(x - first(r), step=negate(step(r)), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::OrdinalRange, x::Integer) = range(first(r) - x, last(r) - x, step=step(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), x::Integer, r::OrdinalRange) = range(x - first(r), x - last(r), step=negate(step(r)))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::AbstractUnitRange, x::Integer) = range(first(r) - x, last(r) - x)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::AbstractUnitRange, x::Real) = range(first(r) - x, length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::StepRangeLen{T}, x::Number) where T =
    StepRangeLen{typeof(T(r.ref)-x)}(r.ref - x, r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), x::Number, r::StepRangeLen{T}) where T =
    StepRangeLen{typeof(x-T(r.ref))}(x - r.ref, negate(r.step), length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r::LinRange, x::Number) = LinRange(r.start - x, r.stop - x, length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), x::Number, r::LinRange) = LinRange(x - r.start, x - r.stop, length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(-), r1::AbstractRange, r2::AbstractRange) = r1 - r2

# at present Base.range_start_step_length(1,0,5) is an error, so for 0 .* (-2:2) we explicitly construct StepRangeLen:
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), x::Number, r::AbstractRange) = StepRangeLen(x*first(r), x*step(r), length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), x::Number, r::StepRangeLen{T}) where {T} =
    StepRangeLen{typeof(x*T(r.ref))}(x*r.ref, x*r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), x::Number, r::LinRange) = LinRange(x * r.start, x * r.stop, r.len)
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), x::AbstractFloat, r::OrdinalRange) =
    Base.range_start_step_length(x*first(r), x*step(r), length(r))  # 0.2 .* (-2:2) needs TwicePrecision
# separate in case of noncommutative multiplication:
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::AbstractRange, x::Number) = StepRangeLen(first(r)*x, step(r)*x, length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::StepRangeLen{T}, x::Number) where {T} =
    StepRangeLen{typeof(T(r.ref)*x)}(r.ref*x, r.step*x, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::LinRange, x::Number) = LinRange(r.start * x, r.stop * x, r.len)
broadcasted(::DefaultArrayStyle{1}, ::typeof(*), r::OrdinalRange, x::AbstractFloat) =
    Base.range_start_step_length(first(r)*x, step(r)*x, length(r))

#broadcasted(::DefaultArrayStyle{1}, ::typeof(/), r::AbstractRange, x::Number) = range(first(r)/x, last(r)/x, length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(/), r::AbstractRange, x::Number) = range(first(r)/x, step=step(r)/x, length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(/), r::StepRangeLen{T}, x::Number) where {T} =
    StepRangeLen{typeof(T(r.ref)/x)}(r.ref/x, r.step/x, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(/), r::LinRange, x::Number) = LinRange(r.start / x, r.stop / x, r.len)

broadcasted(::DefaultArrayStyle{1}, ::typeof(\), x::Number, r::AbstractRange) = range(x\first(r), step=x\step(r), length=length(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(\), x::Number, r::StepRangeLen) = StepRangeLen(x\r.ref, x\r.step, length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(\), x::Number, r::LinRange) = LinRange(x \ r.start, x \ r.stop, r.len)

broadcasted(::DefaultArrayStyle{1}, ::typeof(big), r::UnitRange) = big(r.start):big(last(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(big), r::StepRange) = big(r.start):big(r.step):big(last(r))
broadcasted(::DefaultArrayStyle{1}, ::typeof(big), r::StepRangeLen) = StepRangeLen(big(r.ref), big(r.step), length(r), r.offset)
broadcasted(::DefaultArrayStyle{1}, ::typeof(big), r::LinRange) = LinRange(big(r.start), big(r.stop), length(r))

## CartesianIndices
broadcasted(::typeof(+), I::CartesianIndices{N}, j::CartesianIndex{N}) where {N} =
    CartesianIndices(map((rng, offset)->rng .+ offset, I.indices, Tuple(j)))
broadcasted(::typeof(+), j::CartesianIndex{N}, I::CartesianIndices{N}) where {N} =
    I .+ j
broadcasted(::typeof(-), I::CartesianIndices{N}, j::CartesianIndex{N}) where {N} =
    CartesianIndices(map((rng, offset)->rng .- offset, I.indices, Tuple(j)))
function broadcasted(::typeof(-), j::CartesianIndex{N}, I::CartesianIndices{N}) where {N}
    diffrange(offset, rng) = range(offset-last(rng), length=length(rng), step=step(rng))
    Iterators.reverse(CartesianIndices(map(diffrange, Tuple(j), I.indices)))
end

## In specific instances, we can broadcast masked BitArrays whole chunks at a time
# Very intentionally do not support much functionality here: scalar indexing would be O(n)
struct BitMaskedBitArray{N,M}
    parent::BitArray{N}
    mask::BitArray{M}
    BitMaskedBitArray{N,M}(parent, mask) where {N,M} = new(parent, mask)
end
function BitMaskedBitArray(parent::BitArray{N}, mask::BitArray{M}) where {N,M}
    @boundscheck checkbounds(parent, mask)
    BitMaskedBitArray{N,M}(parent, mask)
end
@propagate_inbounds dotview(B::BitArray, i::BitArray) = BitMaskedBitArray(B, i)
Base.show(io::IO, B::BitMaskedBitArray) = foreach(arg->show(io, arg), (typeof(B), (B.parent, B.mask)))
# Override materialize! to prevent the BitMaskedBitArray from escaping to an overrideable method
materialize!(B::BitMaskedBitArray, bc::Broadcasted{<:Any,<:Any,typeof(identity),Tuple{Bool}}) = fill!(B, bc.args[1])
materialize!(B::BitMaskedBitArray, bc::Broadcasted{<:Any}) = materialize!(SubArray(B.parent, to_indices(B.parent, (B.mask,))), bc)
function Base.fill!(B::BitMaskedBitArray, b::Bool)
    Bc = B.parent.chunks
    Ic = B.mask.chunks
    @inbounds if b
        for i = 1:length(Bc)
            Bc[i] |= Ic[i]
        end
    else
        for i = 1:length(Bc)
            Bc[i] &= ~Ic[i]
        end
    end
    return B
end



############################################################

# x[...] .= f.(y...) ---> broadcast!(f, dotview(x, ...), y...).
# The dotview function defaults to getindex, but we override it in
# a few cases to get the expected in-place behavior without affecting
# explicit calls to view.   (All of this can go away if slices
# are changed to generate views by default.)

@propagate_inbounds dotview(args::Vararg{Any,N}) where {N} = Base.maybeview(args...)

############################################################
# The parser turns @. into a call to the __dot__ macro,
# which converts all function calls and assignments into
# broadcasting "dot" calls/assignments:

dottable(x) = false # avoid dotting spliced objects (e.g. view calls inserted by @view)
# don't add dots to dot operators
dottable(x::Symbol) = (!isoperator(x) || first(string(x)) != '.' || x === :..) && x !== :(:)
dottable(x::Expr) = x.head !== :$
undot(x) = x
function undot(x::Expr)
    if x.head === :.=
        Expr(:(=), x.args...)
    elseif x.head === :block # occurs in for x=..., y=...
        Expr(:block, Base.mapany(undot, x.args)...)
    else
        x
    end
end
__dot__(x) = x
function __dot__(x::Expr)
    dotargs = Base.mapany(__dot__, x.args)
    if x.head === :call && dottable(x.args[1])
        Expr(:., dotargs[1], Expr(:tuple, dotargs[2:end]...))
    elseif x.head === :comparison
        Expr(:comparison, (iseven(i) && dottable(arg) && arg isa Symbol && isoperator(arg) ?
                               Symbol('.', arg) : arg for (i, arg) in pairs(dotargs))...)
    elseif x.head === :$
        x.args[1]
    elseif x.head === :let # don't add dots to `let x=...` assignments
        Expr(:let, undot(dotargs[1]), dotargs[2])
    elseif x.head === :for # don't add dots to for x=... assignments
        Expr(:for, undot(dotargs[1]), dotargs[2])
    elseif (x.head === :(=) || x.head === :function || x.head === :macro) &&
           Meta.isexpr(x.args[1], :call) # function or macro definition
        Expr(x.head, x.args[1], dotargs[2])
    elseif x.head === :(<:) || x.head === :(>:)
        tmp = x.head === :(<:) ? :.<: : :.>:
        Expr(:call, tmp, dotargs...)
    else
        head = String(x.head)::String
        if last(head) == '=' && first(head) != '.' || head == "&&" || head == "||"
            Expr(Symbol('.', head), dotargs...)
        else
            Expr(x.head, dotargs...)
        end
    end
end
"""
    @. expr

Convert every function call or operator in `expr` into a "dot call"
(e.g. convert `f(x)` to `f.(x)`), and convert every assignment in `expr`
to a "dot assignment" (e.g. convert `+=` to `.+=`).

If you want to *avoid* adding dots for selected function calls in
`expr`, splice those function calls in with `\$`.  For example,
`@. sqrt(abs(\$sort(x)))` is equivalent to `sqrt.(abs.(sort(x)))`
(no dot for `sort`).

(`@.` is equivalent to a call to `@__dot__`.)

# Examples
```jldoctest
julia> x = 1.0:3.0; y = similar(x);

julia> @. y = x + 3 * sin(x)
3-element Vector{Float64}:
 3.5244129544236893
 4.727892280477045
 3.4233600241796016
```
"""
macro __dot__(x)
    esc(__dot__(x))
end


function broadcasted_kwsyntax(f, args::Vararg{Any,N}; kwargs...) where {N}
    if isempty(kwargs) # some BroadcastStyles dispatch on `f`, so try to preserve its type
        return broadcasted(f, args...)
    else
        return broadcasted((args...) -> f(args...; kwargs...), args...)
    end
end
function broadcasted(f, args::Vararg{Any,N}) where {N}
    args′ = map(broadcastable, args)
    return broadcasted(combine_styles(args′...), f, args′...)
end
broadcasted(::S, f, args::Vararg{Any,N}) where {S<:BroadcastStyle,N} = Broadcasted{S}(f, args)

"""
    BroadcastFunction{F} <: Function

Represents the "dotted" version of an operator, which broadcasts the operator over its
arguments, so `BroadcastFunction(op)` is functionally equivalent to `(x...) -> (op).(x...)`.

Can be created by just passing an operator preceded by a dot to a higher-order function.

# Examples
```jldoctest
julia> a = [[1 3; 2 4], [5 7; 6 8]];

julia> b = [[9 11; 10 12], [13 15; 14 16]];

julia> map(.*, a, b)
2-element Vector{Matrix{Int64}}:
 [9 33; 20 48]
 [65 105; 84 128]

julia> Base.BroadcastFunction(+)(a, b) == a .+ b
true
```

!!! compat "Julia 1.6"
    `BroadcastFunction` and the standalone `.op` syntax are available as of Julia 1.6.
"""
struct BroadcastFunction{F} <: Function
    f::F
end

(op::BroadcastFunction)(args::Vararg{Any,N}; kwargs...) where {N} = op.f.(args...; kwargs...)

function Base.show(io::IO, op::BroadcastFunction)
    print(io, BroadcastFunction, '(')
    show(io, op.f)
    print(io, ')')
    nothing
end
Base.show(io::IO, ::MIME"text/plain", op::BroadcastFunction) = show(io, op)

end # module
