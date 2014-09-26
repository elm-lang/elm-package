module Diff.Display where

-- | Pretty-printing function for module comparison entries
showEntry :: ComparisonEntry -> [String]
showEntry (ComparisonEntry _ raw1 raw2 s) =
  case (s, raw2) of
    (Existing _, Just doc2) ->
      [ "  - " ++ doc2
      , "  + " ++ raw1
      , ""
      ]
    _ -> ["    " ++ raw1]

{-| Add an (indented) top line for non-empty list of strings.
Leave empty list of strings as it is
-}
addPrefix :: Int -> String -> [String] -> [String]
addPrefix len pref ls =
  case ls of
    [] -> []
    _ -> ((take len (repeat ' ') ++ pref) : ls)

-- | Pretty-print module comparison given as a list of entries
renderEntries :: [ComparisonEntry] -> [String]
renderEntries entries =
  concat [ addPrefix 2 "Added:" $ getDocs Added
         , addPrefix 2 "Changed:" $
           getManyDocs [Existing Compatible, Existing Incompatible]
         , addPrefix 2 "Removed:" $ getDocs Removed
         ]
  where
    sortedEntries = foldr insertItem Map.empty entries

    getDocs key = Maybe.fromMaybe [] $ Map.lookup key sortedEntries
    getManyDocs keys = concatMap getDocs keys

    insertItem entry m =
      case state entry of
        Existing Same -> m
        st -> Map.insertWith (++) st (showEntry entry) m

-- | Pretty-print comparison of whole docs.json
renderDocsComparison :: Map.Map String [ComparisonEntry] -> [String]
renderDocsComparison = Map.foldrWithKey attach []
  where
    attach name entries ls =
      addPrefix 0 ("Module " ++ name) (renderEntries entries) ++ ls

