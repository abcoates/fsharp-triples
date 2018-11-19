# F# Discriminated Unions for representing RDF Triples and Related SPARQL Constructs

This is just an experiment in what would be the best way, in F#, to write code for processing RDF triples - along the lines of doing something more-or-less SPARQL-compatible.

In RDF, you have the types URI \< Resource \< Value (in terms of breadth).  What would be most convenient is if you could extend a discriminated union (DU) for URI to get Resource, and extend Resource to get Value.  Unfortunately, you can't extend DUs in that way in F#.

Instead, I have ended up using 3-tuples with the broadest possible type, and then an 'isValidTuple' method to determine if the 3-type has the correct structure for an RDF triple.

Currently, all of the code is contained in 'Test.fs'.