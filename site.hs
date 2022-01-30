{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid                    ( mappend )
import           Hakyll
import           System.FilePath


config :: Configuration
config = defaultConfiguration
    { deployCommand =
        "rsync -avzP -e \"ssh\" _site/ apajocnb@www.pankoff.net:www.pankoff.net"
    }


main :: IO ()
main = hakyllWith config $ do
    match "img/me.jpg" $ do
        route $ setExtension ".webp"
        compile $ getResourceLBS >>= withItemBody
            (unixFilterLBS
                "cwebp"
                ["-resize", "160", "160", "-q", "90", "-o", "-", "--", "-"]
            )

    match "img/**" $ do
        route idRoute
        compile copyFileCompiler

    match "font-awesome/**" $ do
        route idRoute
        compile copyFileCompiler

    match "favicon/**" $ do
        route $ customRoute (takeFileName . toFilePath)
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "httpd-conf/httpd.conf" $ do
        route $ constRoute ".htaccess"
        compile copyFileCompiler

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

    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            pages <- loadAll "pages/**"
            let sitemapCtx = listField
                    "entries"
                    (constField "host" "https://pankoff.net" <> defaultContext)
                    (return pages)

            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
                >>= cleanIndexHtmls

    match "templates/*" $ compile templateBodyCompiler


-- https://github.com/crodjer/rohanjain.in/blob/587d63bd3c9b9afafe3cf55ac5e4de751a5f8289/content/old/hakyll-clean-urls.md
cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"