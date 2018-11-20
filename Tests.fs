// Test of using F# discriminated unions for representing RDF triples + related SPARQL concepts
// Uses 'active patterns' to simulate subsets of discriminated unions.
module FsharpTripleTests

open System
open Xunit

type Pattern = // based on what you need to represent a SPARQL query
| Obj of Object
| Blank of string
| IRI of string
| Function of (Object -> bool)
| Variable of string

let (|URI|_|) (p:Pattern) =
    match p with
    | IRI _ -> Some(p)
    | _ -> None

let (|Resource|_|) (p:Pattern) =
    match p with
    | URI p -> Some(p)
    | Blank _ -> Some(p)
    | _ -> None

let (|Value|_|) (p:Pattern) =
    match p with
    | Resource p -> Some(p)
    | Obj _ -> Some(p)
    | _ -> None

type Triple = Pattern * Pattern * Pattern // would ideally be Resource * IRI * Value - see 'isValidTriple'
type TripleList = Triple list
type TriplePattern = Pattern * Pattern * Pattern

let (|ValidTriple|_|) (t:Triple) =
    match t with
    | (Resource s, URI p, Value o) -> Some(t)
    | _ -> None

type FilterResult = { triples: TripleList; variables: Map<string, Pattern list> }

let initFilterResult (tl:TripleList): FilterResult = { triples = tl; variables = List.empty |> Map.ofList }

let emptyFilterResult: FilterResult = List.empty |> initFilterResult

// Always construct a new triple using 'buildTriple'
let buildTriple (subj: Pattern) (pred: Pattern) (obj: Pattern): Triple option =
    let triple: Triple = (subj, pred, obj)
    match triple with
    | ValidTriple triple -> Some(triple)
    | _ -> None

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
