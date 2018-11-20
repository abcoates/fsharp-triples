// Test of using F# discriminated unions for representing RDF triples + related SPARQL concepts
module FsharpTripleTests

open System
open Xunit

type Pattern = // based on what you need to represent a SPARQL query
| Obj of Object
| Blank of string
| IRI of string
| Function of (Object -> bool)
| Variable of string
    member x.isURI: bool =
        match x with
        | IRI _ -> true
        | _ -> false
    member x.isResource: bool =
        match x with
        | x when x.isURI -> true
        | Blank _ -> true
        | _ -> false
    member x.isValue: bool =
        match x with
        | x when x.isResource -> true
        | Obj _ -> true
        | _ -> false

type Triple = Pattern * Pattern * Pattern // would ideally be Resource * IRI * Value - see 'isValidTriple'
type TripleList = Triple list
type TriplePattern = Pattern * Pattern * Pattern

let isValidTriple (t:Triple): bool =
    match t with
    | (s, p, o) when s.isResource && p.isURI && o.isValue -> true
    | _ -> false

type FilterResult = { triples: TripleList; variables: Map<string, Pattern list> }

let initFilterResult (tl:TripleList): FilterResult = { triples = tl; variables = List.empty |> Map.ofList }

let emptyFilterResult: FilterResult = List.empty |> initFilterResult

// Always construct a new triple using 'buildTriple'
let buildTriple (subj: Pattern) (pred: Pattern) (obj: Pattern): Triple option =
    let triple: Triple = (subj, pred, obj)
    if (isValidTriple triple)
    then Some(triple)
    else None

let rec applyPattern (triplePattern:TriplePattern) (previousResults:FilterResult): FilterResult =
    emptyFilterResult // TODO: put in the real code here

// Functions for chaining the application of filters
let (|>>) (tp:TriplePattern) (tl:TripleList) = applyPattern tp (tl |> initFilterResult) // TripleList to FilterResult
let (>>>) = applyPattern // FilterResult to FilterResult
let (>>|) (fr:FilterResult) = fr.triples // FilterResult to TripleList

[<Fact>]
let ``a triple can be constructed`` () =
    do
        let t1: Triple option = buildTriple (Blank "_AAA") (IRI "http://example.com/verb#hasCount") (Obj 23)
        Assert.True(t1.IsSome)
        let t1List: TripleList = t1 |> Option.toList
        Assert.Equal(t1List |> List.length, 1)
