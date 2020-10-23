module Tools.REPL where

import Control.Monad.Trans
import Data.List (isPrefixOf)
import Data.Monoid
import System.Console.Repline
import System.Process (callCommand)
import Text.Read (get)

type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Commands
help :: String -> Repl ()
help args = liftIO $ putStrLn $ "Help: " ++ args

say :: String -> Repl ()
say args = do
  _ <- liftIO $ callCommand $ "cowsay" ++ " " ++ args
  return ()

load :: FilePath -> Repl ()
load args = do
  contents <- liftIO $ readFile args
  liftIO $ putStrLn contents

-- Options
opts :: [(String, String -> Repl ())]
opts =
  [ ("?", help), -- :help
    ("load", load), -- :load
    ("say", say) -- :say
  ]

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["?", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

-- Completer
defaultMatcher :: (MonadIO m) => [([Char], CompletionFunc m)]
defaultMatcher =
  [ -- Commands
    (":load", fileCompleter),
    (":?", wordCompleter completer)
  ]

byWord :: Monad m => WordCompleter m
byWord n = do
  let names = fmap ((":" <>) . fst) opts
  return $ filter (isPrefixOf n) names

-- Initialiser function
ini :: Repl ()
ini = liftIO $ putStrLn "This is Horus, version 0.1.0: https://github.com/deividrvale/horus    use :? for help"

-- Finaliser function
final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure "λ> "
customBanner MultiLine = pure "| "

repl :: IO ()
repl =
  evalReplOpts $
    ReplOpts
      { banner = customBanner,
        command = cmd,
        options = opts,
        prefix = Just ':',
        multilineCommand = Just "paste",
        tabComplete = (Prefix (wordCompleter byWord) defaultMatcher),
        initialiser = ini,
        finaliser = final
      }
