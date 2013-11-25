{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Registry.Generate.Html where

import Control.Monad.Error
import qualified Data.List as List
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import System.FilePath as FP
import System.Directory

import qualified Get.Utils as Utils

generate :: FilePath -> ErrorT String IO ()
generate path =
  do Utils.run "elm" ["--make","--runtime=/elm-runtime.js", "--build-dir=.", path]
     liftIO $ adjustHtmlFile $ FP.replaceExtension path "html"

adjustHtmlFile :: FilePath -> IO ()
adjustHtmlFile file =
  do src <- readFile file
     let (before,after) =
             length src `seq`
             List.break (List.isInfixOf "<title>") (lines src)
     removeFile file
     writeFile (replaceExtension file "elm") $
         unlines (before ++ [style] ++ after ++ [analytics])
  where
    style = 
        unlines . map ("    "++) $
        [ "<style type=\"text/css\">"
        , "  a:link {text-decoration: none; color: rgb(15,102,230);}"
        , "  a:visited {text-decoration: none}"
        , "  a:active {text-decoration: none}"
        , "  a:hover {text-decoration: underline; color: rgb(234,21,122);}"
        , "  body { font-family: calibri, verdana, helvetica, arial }"
        , "</style>" ]

-- | Add analytics to a page.
analytics :: String
analytics = renderHtml $
    H.script ! A.type_ "text/javascript" $
         "var _gaq = _gaq || [];\n\
         \_gaq.push(['_setAccount', 'UA-25827182-1']);\n\
         \_gaq.push(['_setDomainName', 'elm-lang.org']);\n\
         \_gaq.push(['_trackPageview']);\n\
         \(function() {\n\
         \  var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;\n\
         \  ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';\n\
         \  var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);\n\
         \})();"
