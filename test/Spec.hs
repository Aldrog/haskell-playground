import Maze
import Direction
import CellMaze

emptyMaze = makeMaze $ AD True $ AD True $ AD True $ AD True AD0

main :: IO ()
main = do
  let m = emptyMaze (Width 4) (Height 4)
  putStrLn $ show $ checkMove m (Position (0, 0)) Direction.Left
  putStrLn $ show $ checkMove m (Position (1, 0)) Direction.Right
  putStrLn $ show $ checkMove m (Position (0, 1)) Direction.Up
  putStrLn $ show $ checkMove m (Position (1, 1)) Direction.Down
