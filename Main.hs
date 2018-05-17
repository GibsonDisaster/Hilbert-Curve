module Main where
  import Graphics.Gloss

  type Coord = (Int, Int)

  data Hilbert = First [Coord]
               | Second [Coord]
               | Nth [Coord]
               deriving (Show, Eq)

  renderHilbert :: Hilbert -> Picture
  renderHilbert (First pts) = pics
    where
      pics = translate 0 0 $ color white $ polygon pts

  render :: Picture
  render = renderHilbert (First [(0, 0), ()])

  main :: IO ()
  main = display (InWindow "Hilbert Curve" (0, 0) (300, 300)) black render