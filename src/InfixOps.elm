import open Docs
import open ColorScheme
import Search
import String
import Window
import RawInfixes (table)

main = documentation "Infix Operators" entries
           <~ Window.dimensions
            ~ Search.box
            ~ Search.results

toLink (name,op) =
    let name' = String.map (\c -> if c == '.' then '/' else c) name
    in  monospace <| Text.link ("/library/" ++ name' ++ ".elm#" ++ op) (toText op)

assoc ops =
    if isEmpty ops then spacer 10 10 else
        text . concat . intersperse (toText ", ") <| map toLink ops

makeRow num entries w =
    let number = case num of
                   Nothing -> spacer 40 40
                   Just n -> container 40 40 middle (plainText (show n))
    in  container w 40 middle . flow right <|
        number :: color mediumGrey (spacer 1 40) :: map (container 140 40 middle) entries

prec n row = makeRow (Just n) (map assoc row)

intro = [markdown|
This table shows the relationships between all of the infix operators
in Elm&rsquo;s standard library.

[Precedence](http://en.wikipedia.org/wiki/Order_of_operations) indicates the
order of operations. Each operator has a precedence from zero to nine, where
nine happens first, then eight, etc.
[Associativity](http://en.wikipedia.org/wiki/Operator_associativity) determines
how parentheses get inserted when operators have the same precedence.
|]

entries = flip width intro ::
          makeRow Nothing (map plainText ["left","non","right"]) ::
          (\w -> container w 1 midTop <| color mediumGrey <| spacer 460 1) ::
          reverse (zipWith prec [0..9] table)
