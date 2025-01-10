import Data.Char (toLower)
import Data.List (elemIndex)

data Todo = Todo {description :: String, completed :: Bool}
  deriving (Show, Eq)

todos :: [Todo]
todos = []

addTodo :: String -> [Todo] -> [Todo]
addTodo desc todos = Todo desc False : todos

completeTodo :: Int -> [Todo] -> [Todo]
completeTodo index todos =
  case elemIndex (todos !! index) todos of
    Just i ->
      let (x, (_ : xs)) = splitAt i todos
          updatedTodo = (todos !! i) {completed = True}
       in x ++ updatedTodo : xs
    Nothing -> todos

removeTodo :: Int -> [Todo] -> [Todo]
removeTodo index todos =
  case elemIndex (todos !! index) todos of
    Just i -> let (x, (_ : xs)) = splitAt i todos in x ++ xs
    Nothing -> todos

mainWithTodos :: [Todo] -> IO ()
mainWithTodos todos = do
  putStrLn "enter a command (add, show, complete, remove, quit): "
  command <- getLine

  case map toLower command of
    "add" -> do
      putStrLn "enter a task: "
      task <- getLine
      let newTodos = addTodo task todos
      print (Todo task False)
      mainWithTodos newTodos
    "show" -> do
      if null todos
        then putStrLn "You have no todos yet"
        else do
          putStrLn "Todos: "
          mapM_ printTodo (zip [0 ..] todos)
          mainWithTodos todos
    "complete" -> do
      putStrLn "Enter the number of the task to complete: "
      taskNumStr <- getLine
      case reads taskNumStr of
        [(taskNum, "")] ->
          if taskNum > 0 && taskNum <= length todos
            then mainWithTodos (completeTodo (taskNum - 1) todos)
            else do
              putStrLn "Invalid task number"
              mainWithTodos todos
        _ -> do
          putStrLn "Invalid input"
          mainWithTodos todos
    "remove" -> do
      putStrLn "Enter the number of the task to remove: "
      taskNumStr <- getLine
      case reads taskNumStr of
        [(taskNum, "")] ->
          if taskNum > 0 && taskNum <= length todos
            then mainWithTodos (removeTodo (taskNum - 1) todos)
            else do
              putStrLn "Invalid task number"
              mainWithTodos todos
        _ -> do
          putStrLn "Invalid task number"
          mainWithTodos todos
    "quit" -> return ()
    _ -> do
      putStrLn "invalid command"
      mainWithTodos todos

printTodo :: (Int, Todo) -> IO ()
printTodo (index, todo) =
  putStrLn $
    show (index + 1)
      ++ ". "
      ++ description todo
      ++ " (completed: "
      ++ show (completed todo)
      ++ ")"

main :: IO ()
main = mainWithTodos []