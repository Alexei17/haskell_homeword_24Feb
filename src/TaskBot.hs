module TaskBot
  ( runBot
  ) where

import Consts
import System.IO
import Data.List

type Task = String
type UserName = String
type BotName = String

-- | Run task bot.
-- Commands:
-- /list -- show task list
-- /complete -- complete the last task
-- /exit -- stop bot
-- Any other input is considered as new task.
runBot :: IO ()
runBot = do
  -- disable buffering for stdout
  hSetBuffering stdout NoBuffering
  putStrLn namePrompt
  name <- getLine
  putStrLn botNamePrompt
  botName <- getLine
  go name botName []
  where
    -- Helper function to interact with user and update tasks list
    go :: UserName -> BotName -> [Task] -> IO ()
    go name botName taskList = do
      putStr $ name ++ "> "
      str <- getLine
      if (str == "/exit")
        then putStrLn goodbyeMsg
        else do
          -- process input unless it is an "/exit" command
          let (output, newTaskList) = processCommand str taskList botName
          putStrLn (botName ++ "> " ++ output) -- didn't want to hardcode but I've struggled enough D:
          go name botName newTaskList

-- | Process user input. Returns output string to be printed by bot and
-- updated list of tasks in a tuple.
processCommand :: String -> [Task] -> BotName -> (String, [Task])
processCommand cmd prevTaskList botName = case cmd of
  "/list" -> cmdList prevTaskList
  "/complete" -> cmdComplete prevTaskList botName
  "/delete" -> (botName ++ "> ", []) -- Delete all tasks from list, just returns an empty list.
  _ -> addTask cmd prevTaskList

-- | Command to show tasks list.
cmdList :: [Task] -> (String, [Task])
cmdList tasks = ("\n" ++ returnNumberedString tasks, tasks) -- \n so it doesn't start on the same line as botName> ...

returnNumberedString :: [Task] -> String
returnNumberedString taskList = intercalate "\n" (zipWith (\n task -> (show n) ++ ". " ++ task) [1..length(taskList)] taskList)
-- interclate converts a list to a string seperating it with a new line.
-- show converts Int to [Char]

-- | Command to complete the last task.
cmdComplete :: [Task] -> BotName -> (String, [Task])
cmdComplete [] botName = (botName ++ "> ", [])
cmdComplete (_:xs) _ = (completeMsg, xs)

-- | Add new task to tasks list.
addTask :: String -> [Task] -> (String, [Task])
addTask task l = (newTaskMsg, task:l)
