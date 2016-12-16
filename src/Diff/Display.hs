{-# LANGUAGE OverloadedStrings #-}
module Diff.Display (packageChanges) where

import Data.Char (isDigit)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text (Text)
import Text.PrettyPrint ((<+>), (<>))
import qualified Text.PrettyPrint as P

import qualified Elm.Compiler.Type as Type
import qualified Diff.Compare as D



-- DISPLAY DIFF


packageChanges :: D.PackageChanges -> String
packageChanges pkgChanges@(D.PackageChanges added changed removed) =
    "This is a " ++ show (D.packageChangeMagnitude pkgChanges) ++ " change.\n\n"
    ++ showAdded
    ++ showRemoved
    ++ showChanged
  where
    showRemoved =
      if null removed then
        ""
      else
        "------ Removed modules - MAJOR ------\n"
        ++ concatMap ((++) "\n    " . Text.unpack) removed
        ++ "\n\n\n"

    showAdded =
      if null added then
        ""
      else
        "------ Added modules - MINOR ------\n"
        ++ concatMap ((++) "\n    " . Text.unpack) added
        ++ "\n\n\n"

    showChanged =
      if Map.null changed then
        ""
      else
        concatMap moduleChanges (Map.toList changed)


moduleChanges :: (Text, D.ModuleChanges) -> String
moduleChanges (name, changes) =
    "------ Changes to module " ++ Text.unpack name ++ " - " ++ show magnitude ++ " ------"
    ++ display "Added" adtAdd aliasAdd valueAdd
    ++ display "Removed" adtRemove aliasRemove valueRemove
    ++ display "Changed" adtChange aliasChange valueChange
    ++ "\n\n\n"
  where
    magnitude =
        D.moduleChangeMagnitude changes

    (adtAdd, adtChange, adtRemove) =
        changesToDocs unionDoc (D.adtChanges changes)

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



-- HELPER


chunk :: Text -> P.Doc
chunk txt =
  P.text (Text.unpack txt)



-- PRETTY PRINTING


unionDoc :: Text -> ([Text], Map.Map Text [Type.Type]) -> P.Doc
unionDoc name (tvars, ctors) =
    P.hang setup 4 (P.sep (zipWith (<+>) separators ctorDocs))
  where
    setup =
        P.text "type" <+> chunk name <+> P.hsep (map chunk tvars)

    separators =
        map P.text ("=" : repeat "|")

    ctorDocs =
        map ctorDoc (Map.toList ctors)

    ctorDoc (ctor, tipes) =
        P.hsep (chunk ctor : map parenDoc tipes)


aliasDoc :: Text -> ([Text], Type.Type) -> P.Doc
aliasDoc name (tvars, tipe) =
    P.hang (setup <+> P.equals) 4 (typeDoc tipe)
  where
    setup =
        P.text "type" <+> P.text "alias" <+> chunk name <+> P.hsep (map chunk tvars)


valueDoc :: Text -> Type.Type -> P.Doc
valueDoc name tipe =
    chunk name <+> P.colon <+> typeDoc tipe


parenDoc = generalTypeDoc True
typeDoc = generalTypeDoc False

generalTypeDoc :: Bool -> Type.Type -> P.Doc
generalTypeDoc parens tipe =
    case tipe of
      Type.Var x ->
        chunk x

      Type.Type name ->
        chunk name

      Type.Lambda t t' ->
          let (args, result) = collectLambdas [t] t'
          in
              (if parens then P.parens else id) $
              foldr arrow (typeDoc result) args

      Type.App t ts ->
          case t : ts of
            [ Type.Type name, tipe ]
              | name == "_List" ->
                  P.lbrack <> typeDoc tipe <> P.rbrack

            Type.Type name : types
              | Text.isPrefixOf "_Tuple" name && Text.all isDigit (Text.drop 6 name) ->
                  P.parens (P.hsep (P.punctuate P.comma (map typeDoc types)))

            types ->
                (if parens then P.parens else id) $
                P.hsep (map parenDoc types)

      Type.Record fields maybeExt ->
          P.sep [ P.hang start 2 fieldDocs, P.rbrace ]
        where
          start =
              case maybeExt of
                Nothing -> P.lbrace
                Just ext -> P.lbrace <+> typeDoc ext <+> P.text "|"

          fieldDocs =
              P.sep (P.punctuate P.comma (map fieldDoc fields))

          fieldDoc (name, tipe) =
              chunk name <+> P.colon <+> typeDoc tipe


arrow :: Type.Type -> P.Doc -> P.Doc
arrow arg result =
    argDoc <+> P.text "->" <+> result
  where
    argDoc =
        case arg of
          Type.Lambda _ _ -> P.parens (typeDoc arg)
          _ -> typeDoc arg


collectLambdas :: [Type.Type] -> Type.Type -> ([Type.Type], Type.Type)
collectLambdas args result =
    case result of
      Type.Lambda t t' -> collectLambdas (t:args) t'
      _ -> (reverse args, result)

