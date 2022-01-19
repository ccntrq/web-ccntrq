{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                    ( mappend )
import           Hakyll
import           System.FilePath


config :: Configuration
config = defaultConfiguration
    { deployCommand =
        "rsync -ave \"ssh\" _site/* apajocnb@www.pankoff.net:www.pankoff.net"
    }


main :: IO ()
main = hakyllWith config $ do
    match "img/**" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "httpd-conf/httpd.conf" $ do
        route $ constRoute ".htaccess"
        compile compressCssCompiler

    match "pages/*" $ do

        route $ customRoute
            (\identifier ->
                let path = toFilePath identifier
                in  if path == "pages/about-me.md"
                        then "index.html"
                        else replaceExtension path "html"
            )
        compile
            $   pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


    match "templates/*" $ compile templateBodyCompiler
