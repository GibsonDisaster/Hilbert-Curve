module Main where
  import Graphics.Gloss

  {-
    TODO:
      Code to take a Coord and transform it into an 2nd Gen hilbert Curve
      make function to transform hilbert curve into new curve made of 4 originals
      connect the lines function
  -}

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
      bl = First [[(0.0, (-150.0)), (0.0, 0.0)], [(0.0, 150.0), (150.0, 150.0)], [(150.0, 0.0), (150.0, (-150.0))]]
      br = First []

  rotateHilbert :: Hilbert -> Int -> Hilbert

  makeLines :: [Coord] -> Picture
  makeLines (s:e:r) = color white $ line [s, e]

  renderHilbert :: Hilbert -> Picture
  renderHilbert (First pts) = Pictures $ pics
    where
      pics = map makeLines pts
  renderHilbert (Second fs) = Pictures $ map renderHilbert fs

  render :: Picture
  render = renderHilbert hilbertData

  main :: IO ()
  main = display (InWindow "Hilbert Curve" (900, 900) (300, 300)) black render