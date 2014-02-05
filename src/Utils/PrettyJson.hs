module Utils.PrettyJson where

import           Text.JSON
import           Text.JSON.Pretty

value :: JSValue -> Doc
value val =
    case val of
      JSNull -> pp_null
      JSBool b -> pp_boolean b
      JSRational asf x -> pp_number asf x
      JSString s -> pp_js_string s
      JSArray vs -> array vs
      JSObject obj -> object (fromJSObject obj)

array :: [JSValue] -> Doc
array [] = lbrack <> rbrack
array vs =
    (if length vs < 2 then sep else vcat) (entries ++ [rbrack])
  where
    entries = zipWith (<+>) (lbrack : repeat comma) (map value vs)

object :: [(String, JSValue)] -> Doc
object [] = lbrace <> rbrace
object assocs =
    (if length assocs < 2 then sep else vcat) (entries ++ [rbrace])
  where
    entries = zipWith (<+>) (lbrace : repeat comma) fields
    fields = map (\(f,v) -> hang (pp_string f <> colon) 4 (value v)) assocs
