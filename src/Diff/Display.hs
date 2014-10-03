module Diff.Display (packageChanges) where

import qualified Data.Map as Map
import Text.PrettyPrint ((<+>), (<>))
import qualified Text.PrettyPrint as P
import qualified Diff.Compare as D
import qualified Diff.Model as M


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
            "Removed modules:"
            ++ concatMap ("\n  " ++) removed
            ++ "\n\n"

    showAdded
        | null added = ""
        | otherwise =
            "Added modules:"
            ++ concatMap ("\n  " ++) added
            ++ "\n\n"

    showChanged
        | Map.null changed = ""
        | otherwise =
            concatMap moduleChanges (Map.toList changed)
            ++ "\n"


moduleChanges :: (String, D.ModuleChanges) -> String
moduleChanges (name, changes) =
    "Changes to module " ++ name ++ " - " ++ show magnitude ++ "\n"
    ++ display "Added" adtAdd aliasAdd valueAdd
    ++ display "Removed" adtRemove aliasRemove valueRemove
    ++ display "Changed" adtChange aliasChange valueChange
    ++ "\n"
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
        P.text "    " <> toDoc name value

    diffed (name, (oldValue, newValue)) =
        P.vcat
        [ P.text "  - " <> toDoc name oldValue
        , P.text "  + " <> toDoc name newValue
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
          "  " ++ categoryName ++ ":"


-- PRETTY PRINTING

adtDoc :: String -> ([String], Map.Map String M.Type) -> P.Doc
adtDoc name (tvars, ctors) =
    P.hang setup 4 (P.sep (zipWith (<+>) separators ctorDocs))
  where
    setup =
        P.text "data" <+> P.text name <+> P.hsep (map P.text tvars)

    separators =
        map P.text ("=" : repeat "|")

    ctorDocs =
        map ctorDoc (Map.toList ctors)

    ctorDoc (ctor, tipe) =
        let (args, _) = collectLambdas [] tipe
        in
            P.hsep (P.text ctor : map parenDoc args)


aliasDoc :: String -> ([String], M.Type) -> P.Doc
aliasDoc name (tvars, tipe) =
    P.hang (setup <+> P.equals) 4 (typeDoc tipe)
  where
    setup =
        P.text "type" <+> P.text name <+> P.hsep (map P.text tvars)


valueDoc :: String -> M.Type -> P.Doc
valueDoc name tipe =
    P.text name <+> P.colon <+> typeDoc tipe


typeDoc :: M.Type -> P.Doc
typeDoc tipe =
    case tipe of
      M.Var x -> P.text x

      M.Type name -> P.text name

      M.Lambda t t' ->
          let (args, result) = collectLambdas [t] t'
          in
              foldr arrow (typeDoc result) args

      M.App t t' ->
          let types = collectApps [t'] t
          in
              P.hsep (map parenDoc types)

      M.Record fields maybeExt ->
          P.sep [ P.hang start 4 fieldDocs, P.rbrace ]
        where
          start =
              case maybeExt of
                Nothing -> P.lbrace
                Just ext -> P.lbrace <+> typeDoc ext <+> P.text "|"

          fieldDocs =
              P.sep (P.punctuate P.comma (map fieldDoc fields))

          fieldDoc (name, tipe) =
              P.text name <+> P.colon <+> typeDoc tipe


arrow :: M.Type -> P.Doc -> P.Doc
arrow arg result =
    argDoc <+> P.text "->" <+> result
  where
    argDoc =
        case arg of
          M.Lambda _ _ -> P.parens (typeDoc arg)
          _ -> typeDoc arg


parenDoc :: M.Type -> P.Doc
parenDoc tipe =
    case tipe of
      M.Lambda _ _ -> P.parens (typeDoc tipe)
      M.App _ _ -> P.parens (typeDoc tipe)
      _ -> typeDoc tipe


collectLambdas :: [M.Type] -> M.Type -> ([M.Type], M.Type)
collectLambdas args result =
    case result of
      M.Lambda t t' -> collectLambdas (t:args) t'
      _ -> (reverse args, result)


collectApps :: [M.Type] -> M.Type -> [M.Type]
collectApps args tipe =
    case tipe of
      M.App t t' ->
          collectApps (t' : args) t

      _ -> tipe : args
