module Hakyll.Web.ImageToWebP where

import qualified Data.ByteString.Lazy          as LBS
import           Hakyll                         ( Compiler
                                                , Item
                                                , getResourceLBS
                                                , unixFilterLBS
                                                , withItemBody
                                                )

imageToWebPCompiler :: Int -> Int -> Compiler (Item LBS.ByteString)
imageToWebPCompiler width height = getResourceLBS >>= withItemBody
    (unixFilterLBS
        "cwebp"
        ["-resize", show width, show height, "-q", "90", "-o", "-", "--", "-"]
    )
