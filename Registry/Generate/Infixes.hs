module Registry.Generate.Infixes (generate) where

import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Exit
import System.FilePath

import qualified Model.Documentation as Docs
import qualified Language.Elm as Elm

generate :: IO ()
generate = do
  json <- BS.readFile =<< Elm.docs
  case Json.eitherDecode json of
    Left err -> print err >> exitFailure
    Right docs ->
        writeFile ("src" </> "RawInfixes.elm") (docsToInfixes docs)

docsToInfixes docs = "module RawInfixes where\n\ntable = " ++ show lists
  where
    lists =
        flip map [0..9] $ \prec ->
            let assocs = Map.findWithDefault Map.empty prec table
            in  flip map ["left","non","right"] $ \assoc ->
                Map.findWithDefault [] assoc assocs
            
    table =
        let combine = uncurry (Map.insertWith (Map.unionWith (++)))
        in  foldr combine Map.empty (concatMap infixes docs)

    infixes doc =
        let add entry (assoc, prec) =
                (prec, Map.singleton assoc [(Docs.moduleName doc, Docs.name entry)])
        in  Maybe.mapMaybe (\entry -> add entry `fmap` Docs.assocPrec entry) (Docs.entries doc)


