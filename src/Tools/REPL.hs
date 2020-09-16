module Tools.REPL where

import Control.Monad.Trans
import Data.List (isPrefixOf)
import System.Console.Repline
import System.Process

type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print input

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = ["kirk", "spock", "mccoy"]
  return $ filter (isPrefixOf n) names

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

say :: String -> Repl ()
say args = do
  _ <- liftIO $ callCommand $ "cowsay -p" ++ " " ++ args
  return ()

opts :: [(String, String -> Repl ())]
opts =
  [ ("help", help . words), -- :help
    ("say", say) -- :say
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "This is Horus - Your favorite Higher-Order Rewriting Tool!"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

repl_alt :: IO ()
repl_alt = evalReplOpts $ ReplOpts
  { banner           = const $ pure ">>> "
  , command          = cmd
  , options          = opts
  , prefix           = Just ':'
  , multilineCommand = Just "paste"
  , tabComplete      = (Word0 completer)
  , initialiser      = ini
  , finaliser        = final
  }

customBanner :: MultiLine -> Repl String
customBanner SingleLine = pure ">>> "
customBanner MultiLine = pure "| "

repl :: IO ()
repl = evalRepl (const $ pure "λ >> ") cmd opts (Just ':') (Just "paste") (Word0 completer) ini final
