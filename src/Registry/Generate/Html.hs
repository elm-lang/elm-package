{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Registry.Generate.Html where

import Control.Monad.Error
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import System.FilePath as FP
import System.Directory
import qualified Data.ByteString.Char8 as BSC

import qualified Utils.Commands as Cmd

generatePublic :: FilePath -> ErrorT String IO ()
generatePublic path =
  do Cmd.run "elm" ["--make","--runtime=/resources/elm-runtime.js"
                   , "--build-dir=.", "--src-dir=src", path]
     liftIO $ removeFile path
     liftIO $ adjustHtmlFile $ FP.replaceExtension path "html"

generateSrc :: FilePath -> ErrorT String IO ()
generateSrc path =
  do Cmd.run "elm" ["--make","--runtime=/resources/elm-runtime.js"
                   , "--build-dir=.", "--src-dir=src", path]
     let old = FP.replaceExtension path "html"
         new = FP.replaceDirectory old "public"
     liftIO $ do
       renameFile old new
       adjustHtmlFile new

adjustHtmlFile :: FilePath -> IO ()
adjustHtmlFile file =
  do src <- BSC.readFile file
     let (before, after) = BSC.breakSubstring "<title>" src
     BSC.writeFile file $ BSC.concat [before, style, after, analytics]

style :: BSC.ByteString
style = 
    "<style type=\"text/css\">\n\
    \  a:link {text-decoration: none; color: rgb(15,102,230);}\n\
    \  a:visited {text-decoration: none}\n\
    \  a:active {text-decoration: none}\n\
    \  a:hover {text-decoration: underline; color: rgb(234,21,122);}\n\
    \  body { font-family: calibri, verdana, helvetica, arial }\n\
    \</style>"

-- | Add analytics to a page.
analytics :: BSC.ByteString
analytics = BSC.pack $ renderHtml $
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
