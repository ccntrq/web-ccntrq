{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid (mappend)
import           Data.String (IsString)
import           Hakyll hiding (host)
import           Hakyll.Web.ImageToWebP
import           System.FilePath
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.List (sortBy)
import           Data.List.Split (splitOn)

config :: Configuration
config = defaultConfiguration { deployCommand =
                                  "rsync -avzP -e \"ssh\" _site/ apajocnb@www.pankoff.net:www.pankoff.net"
                              }

host :: IsString a => a
host = "https://pankoff.net"

main :: IO ()
main = hakyllWith config
  $ do
    match "img/me.jpg" $ defaultWebPRules 160 160
    match "font-awesome/**"
      $ do
        route idRoute
        compile copyFileCompiler
    match "favicon/**"
      $ do
        route $ customRoute (takeFileName . toFilePath)
        compile copyFileCompiler
    match "css/*"
      $ do
        route idRoute
        compile compressCssCompiler
    match "httpd-conf/httpd.conf"
      $ do
        route $ constRoute ".htaccess"
        compile copyFileCompiler
    match "pages/perl-weekly-challenge.md"
      $ do
        route $ constRoute "pages/perl-weekly-challenge/index.html"
        compile
          $ do
            posts <- loadAll "pages/perl-weekly-challenge/*"
            let archiveCtx = listField
                  "challenges"
                  defaultContext
                  (return
                     (sortBy
                        (\a b -> compareWeeklyChallengePaths
                           (toFilePath $ itemIdentifier b)
                           (toFilePath $ itemIdentifier a))
                        posts)) -- TODO: implement ordering
                  `mappend` constField "title" "Archives"
                  `mappend` defaultContext
            pandocCompiler
              >>= loadAndApplyTemplate
                "templates/perl-weekly-challenge.html"
                archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls
    match "pages/**"
      $ do
        route
          $ customRoute
            (\identifier -> let path = toFilePath identifier
                            in if path == "pages/about-me.md"
                               then "index.html"
                               else replaceExtension path "html")
        compile
          $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls
    create ["sitemap.xml"]
      $ do
        route idRoute
        compile
          $ do
            pages <- loadAll ("pages/**" .&&. complement "pages/404.md")
            let sitemapCtx = listField "entries" hostCtx (return pages)
            makeItem ""
              >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx
              >>= cleanIndexHtmls
    create ["robots.txt"]
      $ do
        route idRoute
        compile
          $ makeItem "" >>= loadAndApplyTemplate "templates/robots.txt" hostCtx
    match "templates/*" $ compile templateBodyCompiler

hostCtx :: Context String
hostCtx = constField "host" host <> defaultContext

-- https://github.com/crodjer/rohanjain.in/blob/587d63bd3c9b9afafe3cf55ac5e4de751a5f8289/content/old/hakyll-clean-urls.md
cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern = "/index.html"

    replacement = const "/"

compareWeeklyChallengePaths :: String -> String -> Ordering
compareWeeklyChallengePaths a b = compare (getChallenge a) (getChallenge b)
  where
    getChallenge :: String -> Int
    getChallenge x =
      read $ (!! 1) $ splitOn "-" $ head $ splitOn "." (splitOn "/" x !! 2)