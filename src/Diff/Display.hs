module Diff.Display (packageChanges) where

import Data.Char (isDigit)
import qualified Data.Map as Map
import Text.PrettyPrint ((<+>), (<>))
import qualified Text.PrettyPrint as P

import qualified Elm.Docs as Docs
import qualified Diff.Compare as D


packageChanges :: D.PackageChanges -> String
packageChanges pkgChanges@(D.PackageChanges added changed removed) =
    "This is a " ++ show (D.packageChangeMagnitude pkgChanges) ++ " change.\n\n"
    ++ showAdded
    ++ showRemoved
    ++ showChanged
  where
    showRemoved
        | null removed = ""
        | otherwise =
            "------ Removed modules - MAJOR ------\n"
            ++ concatMap ("\n    " ++) removed
            ++ "\n\n\n"

    showAdded
        | null added = ""
        | otherwise =
            "------ Added modules - MINOR ------\n"
            ++ concatMap ("\n    " ++) added
            ++ "\n\n\n"

    showChanged
        | Map.null changed = ""
        | otherwise =
            concatMap moduleChanges (Map.toList changed)


moduleChanges :: (String, D.ModuleChanges) -> String
moduleChanges (name, changes) =
    "------ Changes to module " ++ name ++ " - " ++ show magnitude ++ " ------"
    ++ display "Added" adtAdd aliasAdd valueAdd
    ++ display "Removed" adtRemove aliasRemove valueRemove
    ++ display "Changed" adtChange aliasChange valueChange
    ++ "\n\n\n"
  where
    magnitude =
        D.moduleChangeMagnitude changes

    (adtAdd, adtChange, adtRemove) =
        changesToDocs adtDoc (D.adtChanges changes)

    (aliasAdd, aliasChange, aliasRemove) =
        changesToDocs aliasDoc (D.aliasChanges changes)

    (valueAdd, valueChange, valueRemove) =
        changesToDocs valueDoc (D.valueChanges changes)


changesToDocs :: (k -> v -> P.Doc) -> D.Changes k v -> ([P.Doc], [P.Doc], [P.Doc])
changesToDocs toDoc (D.Changes added changed removed) =
    ( map indented (Map.toList added)
    , map diffed   (Map.toList changed)
    , map indented (Map.toList removed)
    )
  where
    indented (name, value) =
        P.text "        " <> toDoc name value

    diffed (name, (oldValue, newValue)) =
        P.vcat
        [ P.text "      - " <> toDoc name oldValue
        , P.text "      + " <> toDoc name newValue
        , P.text ""
        ]


display :: String -> [P.Doc] -> [P.Doc] -> [P.Doc] -> String
display categoryName adts aliases values
    | null (adts ++ aliases ++ values) = ""
    | otherwise =
        P.renderStyle (P.style { P.lineLength = 80 }) $
        P.vcat $
            P.text "" : P.text category : adts ++ aliases ++ values
    where
      category =
          "\n    " ++ categoryName ++ ":"


-- PRETTY PRINTING

adtDoc :: String -> ([String], Map.Map String [Docs.Type]) -> P.Doc
adtDoc name (tvars, ctors) =
    P.hang setup 4 (P.sep (zipWith (<+>) separators ctorDocs))
  where
    setup =
        P.text "type" <+> P.text name <+> P.hsep (map P.text tvars)

    separators =
        map P.text ("=" : repeat "|")

    ctorDocs =
        map ctorDoc (Map.toList ctors)

    ctorDoc (ctor, tipes) =
        P.hsep (P.text ctor : map parenDoc tipes)


aliasDoc :: String -> ([String], Docs.Type) -> P.Doc
aliasDoc name (tvars, tipe) =
    P.hang (setup <+> P.equals) 4 (typeDoc tipe)
  where
    setup =
        P.text "type" <+> P.text "alias" <+> P.text name <+> P.hsep (map P.text tvars)


valueDoc :: String -> Docs.Type -> P.Doc
valueDoc name tipe =
    P.text name <+> P.colon <+> typeDoc tipe


parenDoc = generalTypeDoc True
typeDoc = generalTypeDoc False

generalTypeDoc :: Bool -> Docs.Type -> P.Doc
generalTypeDoc parens tipe =
    case tipe of
      Docs.Var x -> P.text x

      Docs.Type name -> P.text name

      Docs.Lambda t t' ->
          let (args, result) = collectLambdas [t] t'
          in
              (if parens then P.parens else id) $
              foldr arrow (typeDoc result) args

      Docs.App t ts ->
          case t : ts of
            [ Docs.Type name, tipe ]
              | name == "_List" ->
                  P.lbrack <> typeDoc tipe <> P.rbrack
                  
            Docs.Type name : types
              | take 6 name == "_Tuple" && all isDigit (drop 6 name) ->
                  P.parens (P.hsep (P.punctuate P.comma (map typeDoc types)))

            types ->
                (if parens then P.parens else id) $
                P.hsep (map parenDoc types)

      Docs.Record fields maybeExt ->
          P.sep [ P.hang start 2 fieldDocs, P.rbrace ]
        where
          start =
              case maybeExt of
                Nothing -> P.lbrace
                Just ext -> P.lbrace <+> typeDoc ext <+> P.text "|"

          fieldDocs =
              P.sep (P.punctuate P.comma (map fieldDoc fields))

          fieldDoc (name, tipe) =
              P.text name <+> P.colon <+> typeDoc tipe


arrow :: Docs.Type -> P.Doc -> P.Doc
arrow arg result =
    argDoc <+> P.text "->" <+> result
  where
    argDoc =
        case arg of
          Docs.Lambda _ _ -> P.parens (typeDoc arg)
          _ -> typeDoc arg


collectLambdas :: [Docs.Type] -> Docs.Type -> ([Docs.Type], Docs.Type)
collectLambdas args result =
    case result of
      Docs.Lambda t t' -> collectLambdas (t:args) t'
      _ -> (reverse args, result)

