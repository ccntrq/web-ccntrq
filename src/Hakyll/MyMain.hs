module Hakyll.MyMain (myHakyll) where

import           Hakyll.Main hiding (verbosity)
import qualified Options.Applicative as OA
import qualified Hakyll.Core.Logger as Logger
import qualified Hakyll.Commands as Check
import           Hakyll.Core.Rules
import           Hakyll.Core.Configuration
import           Tools.FindWeeklyChallenges (generateNewChallengeFiles)
import           Hakyll.Core.Runtime

data MyCommand = HakyllCommand Command
               | GenerateWeeklyChallenges

data MyOptions = MyOptions { verbosity :: Bool, myOptCommand :: MyCommand }

myHakyll :: Configuration -> Rules a -> IO ()
myHakyll conf rules = do
  args <- myParser conf
  case myOptCommand args of
    HakyllCommand com
      -> hakyllWithArgs conf (Options (verbosity args) com) rules
    GenerateWeeklyChallenges -> generateNewChallengeFiles

myParser :: Configuration -> IO MyOptions
myParser conf = OA.customExecParser defaultParserPrefs (myParserInfo conf)

myParserInfo :: Configuration -> OA.ParserInfo MyOptions
myParserInfo conf = OA.info
  (OA.helper <*> myOptionParser conf)
  (OA.fullDesc
   <> OA.progDesc "site - Static site compiler for my personal website created with Hakyll")

myOptionParser :: Configuration -> OA.Parser MyOptions
myOptionParser conf = MyOptions <$> verboseParser <*> myCommandParser conf
  where
    verboseParser = OA.switch
      (OA.long "verbose" <> OA.short 'v' <> OA.help "Run in verbose mode")

myCommandParser :: Configuration -> OA.Parser MyCommand
myCommandParser
  conf = OA.subparser $ foldr ((<>) . produceCommand) mempty commands
  where
    portParser = OA.option
      OA.auto
      (OA.long "port"
       <> OA.help "Port to listen on"
       <> OA.value (previewPort conf))

    hostParser = OA.strOption
      (OA.long "host"
       <> OA.help "Host to bind on"
       <> OA.value (previewHost conf))

    produceCommand (c, a, b) = OA.command c (OA.info (OA.helper <*> a) b)

    commands =
      [ ( "build"
        , HakyllCommand . Build
          <$> OA.flag
            RunModeNormal
            RunModePrintOutOfDate
            (OA.long "dry-run"
             <> OA.help "Don't build, only print out-of-date items")
        , OA.fullDesc <> OA.progDesc "Generate the site")
      , ( "check"
        , HakyllCommand . Check
          <$> OA.switch
            (OA.long "internal-links" <> OA.help "Check internal links only")
        , OA.fullDesc <> OA.progDesc "Validate the site output")
      , ( "clean"
        , pure (HakyllCommand Clean)
        , OA.fullDesc <> OA.progDesc "Clean up and remove cache")
      , ( "deploy"
        , pure (HakyllCommand Deploy)
        , OA.fullDesc <> OA.progDesc "Upload/deploy your site")
      --, ( "preview"
      --  , HakyllCommand . Preview <$> portParser
      --  , OA.fullDesc
      --    <> OA.progDesc "[DEPRECATED] Please use the watch command")
      , ( "rebuild"
        , pure (HakyllCommand Rebuild)
        , OA.fullDesc <> OA.progDesc "Clean and build again")
      , ( "server"
        , HakyllCommand <$> (Server <$> hostParser <*> portParser)
        , OA.fullDesc <> OA.progDesc "Start a preview server")
      , ( "watch"
        , HakyllCommand
          <$> (Watch <$> hostParser
               <*> portParser
               <*> OA.switch
                 (OA.long "no-server"
                  <> OA.help "Disable the built-in web server"))
        , OA.fullDesc
          <> OA.progDesc
            "Autocompile on changes and start a preview server.  You can watch and recompile without running a server with --no-server.")
      , ( "gen-weekly-challenges"
        , pure GenerateWeeklyChallenges
        , OA.fullDesc <> OA.progDesc "Generate weekly challenge files")]
