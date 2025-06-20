# Semantics4PE/While
Big- and small-step semantics for a subset of C


- From the AST of the parsed program, generate a regular expression over the 
alphabet of assignments and expressions of the code.
- Interpret the regular expression either in big steps or small
- The interpreter can be specialised wrt a given AST, then the output predicates renamed 
to make clear their relation to the source code.  See script c2chc.sh, which allows options
for big, small semantics, and transformations of the regular expressions of the form 
(a+b)* to b*(ab*)*


