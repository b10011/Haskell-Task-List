import System.Environment
import System.IO
import System.IO.Error
import System.Exit
import Control.Monad
import Control.Exception

-- Filename to be used as tasklist storage
filename = "mytasks.txt"

-- Get numbered lines
getNumberedLines' :: Int -> [String] -> [String]
getNumberedLines' _ [] = []
getNumberedLines' index linelist = ((show index) ++ ". " ++ (head linelist)) : (getNumberedLines' (succ index) (drop 1 linelist))

-- Caller for previous
getNumberedLines :: String -> String
getNumberedLines linelist = unlines (getNumberedLines' 1 (lines linelist))

-- Remove line by index in list
removeLine :: Int -> [String] -> [String]
removeLine index linelist = (take index linelist) ++ (drop (succ index) linelist)

-- If file does not exist
fileNotFound :: IOError -> IO String
fileNotFound e
  | isDoesNotExistError e = do
      putStrLn ("Could not find file " ++ filename ++ ". Creating it now...")
      writeTasks ""
      return ""
  | otherwise = do putStrLn "Exception related to taskfile. Maybe permission problem."; return ""

-- IO to file with the file closing properly
writeTasks :: String -> IO()
writeTasks tasks = withFile filename WriteMode (\h -> hPutStr h tasks)
readTasks :: IO String
readTasks = (readFile filename) `catch` fileNotFound

-- Passes command to controller
execute :: [String] -> IO()
execute [] = controller [] []
execute args = controller (head args) (tail args)

-- Controller receives command line arguments
controller :: String -> [String] -> IO()

-- Help function
controller "help" [] = do
  putStrLn "Command line arguments:"
  putStrLn "help            -- Show help"
  putStrLn "show            -- Show tasklist"
  putStrLn "add <task>      -- Add task"
  putStrLn "del <number>    -- Delete task by line number (check 'show')"
  putStrLn "del all         -- Delete all tasks"
  putStrLn "quit            -- Quit program (interactive mode)"

-- Show function
controller "show" [] = do
  contents <- readTasks
  if (length contents) == 0
    then putStrLn "You don't have tasks!"
    else do
      putStrLn "Tasks:"
      putStr (getNumberedLines contents)

-- Add function
controller "add" task = do
  contents <- readTasks
  let tasklines = (lines contents)
  seq (length tasklines) (return ()) -- Forcing readFile to be executed before writeTasks (Haskell has lazy IO)
  writeTasks (unlines (tasklines ++ [unwords task]))
  putStrLn ("Added task \"" ++ (unwords task) ++ "\"")

-- Delete all function
controller "del" ["all"] = do
  writeTasks ""
  putStrLn "Removed all tasks!"

-- Delete function
controller "del" [index] = do
  contents <- readTasks
  let i = read index :: Int
  let tasklines = (lines contents)
  if (i >= (length tasklines)+1) || (i <= 0)
    then (putStrLn "Invalid index")
    else do
      writeTasks (unlines (removeLine (pred i) tasklines))
      (putStrLn ("Removed task \"" ++ (tasklines !! (pred i)) ++ "\""))

-- Quit function
controller "quit" [] = do
  putStrLn "Exiting..."
  exitSuccess

-- No arguments specified catcher function
controller [] [] = do
  putStrLn "You did not specify commands!"
  putStrLn "Entering interactive mode...\n"
  controller "help" []

-- Invalid command catcher function
controller _ _ = do
  putStrLn "You gave invalid command line parameters!\n"
  controller "help" []

-- Main
main = do
       -- First try to get args from command line
       args <- getArgs
       execute args
       -- If no args was given we go to interactive mode
       if args == []
         then do
           forever $ do
             putStr "> "
             line <- getLine
             execute (words line)
       else
         (return ())
