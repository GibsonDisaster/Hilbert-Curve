module Main where
  import Graphics.Gloss

  type Coord = (Float, Float)

  data Hilbert = First [[Coord]]
               | Second [Hilbert]
               | Nth [Hilbert]
               deriving (Show, Eq)

  hilbertData :: Hilbert
  hilbertData = Second [tl, tr, bl, br]
    where
      tl = First [[(0.0, 0.0), (0.0, 150.0)], [(0.0, 150.0), (150.0, 150.0)], [(150.0, 150.0), (150.0, 0.0)]]
      tr = First [[(150.0, 0.0), (150.0, 150.0)], [(150.0, 150.0), (300.0, 150.0)], [(300.0, 150.0), (300.0, 0.0)]]
      bl = First []
      br = First []

  makeLines :: [Coord] -> Picture
  makeLines (s:e:r) = color white $ line [s, e]

  renderHilbert :: Hilbert -> Picture
  renderHilbert (First pts) = Pictures $ pics
    where
      pics = map makeLines pts

  render :: Picture
  render = Pictures $ renderHilbert (First [[(0.0, 0.0), (0.0, 150.0)], [(0.0, 150.0), (150.0, 150.0)], [(150.0, 150.0), (150.0, 0.0)]]) : renderHilbert (First [[(150.0, 0.0), (150.0, 150.0)], [(150.0, 150.0), (300.0, 150.0)], [(300.0, 150.0), (300.0, 0.0)]]) : []

  main :: IO ()
  main = display (InWindow "Hilbert Curve" (900, 900) (300, 300)) black render