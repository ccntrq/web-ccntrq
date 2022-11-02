module Hakyll.MyMain (myHakyll) where

import           Hakyll.Main hiding (verbosity)
import qualified Options.Applicative as OA
import           Hakyll.Core.Rules
import           Hakyll.Core.Configuration
import           Tools.FindWeeklyChallenges (generateNewChallengeFiles)
import           Data.Tuple.Extra (second3)

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
    produceCommand (c, a, b) = OA.command c (OA.info (OA.helper <*> a) b)

    commands = (second3 (HakyllCommand <$>) <$> defaultCommands conf)
      ++ [ ( "gen-weekly-challenges"
           , pure GenerateWeeklyChallenges
           , OA.fullDesc <> OA.progDesc "Generate weekly challenge files")]