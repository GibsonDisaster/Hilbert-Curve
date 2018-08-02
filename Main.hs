module Main where
  import Graphics.Gloss
  import qualified Data.Map as M

  {-
    TODO:
      Code to take a Coord and transform it into an 2nd Gen hilbert Curve
      make function to transform hilbert curve into new curve made of 4 originals
      connect the lines function
  -}

  type Coord = (Float, Float)

  data Hilbert = First [Coord]
               | Second [Hilbert]
               | Nth [Hilbert]
               deriving (Show, Eq)

  spacing :: Float
  spacing = 50.0

  (|+|) :: Coord -> Coord -> Coord
  (|+|) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  hilbertGenMap :: Int -> M.Map Float Float
  hilbertGenMap n = M.fromList $ take n (zip [1..] [ 2 ** x | x <- [0, 2..]])

  getNumOfHilberts :: Float -> Float
  getNumOfHilberts n = case M.lookup n (hilbertGenMap (ceiling n)) of
                         Nothing -> 0.0
                         (Just x) -> x

  rotateClockwise :: Hilbert -> Hilbert
  rotateClockwise (First (a:b:c:d:r)) = First $ [a, d, c, b]
  rotateClockwise (Second pts) = Second $ map rotateClockwise pts
  rotateClockwise (Nth pts) = Nth $ map rotateClockwise pts

  rotateCounterClockwise :: Hilbert -> Hilbert
  rotateCounterClockwise (First (a:b:c:d:r)) = First $ [d, a, b, c]
  rotateCounterClockwise (Second pts) = Second $ map rotateClockwise pts
  rotateCounterClockwise (Nth pts) = Nth $ map rotateClockwise pts

  secondGen :: Coord -> Hilbert
  secondGen (x, y) = Second [rotateClockwise (firstGen (x, y)), firstGen (x, y + spacing), firstGen (x + spacing, y + spacing), rotateCounterClockwise (firstGen (x + spacing, 0.0))]

  firstGen :: Coord -> Hilbert
  firstGen (x, y) = First [(x, y), (x, y + spacing), (x + spacing, y + spacing), (x + spacing, y)]

  genHilbert :: Int -> Hilbert
  genHilbert 1 = firstGen (0.0, 0.0)
  genHilbert n = Nth (replicate 4 (genHilbert (n - 1)))
    where
      y = (ceiling (getNumOfHilberts (fromIntegral n :: Float))) `div` 4

  moveHilbert :: Coord -> Hilbert -> Hilbert
  moveHilbert (xv, yv) (First pts) = First $ map (\(x, y) -> (x + xv, y + yv)) pts
  moveHilbert v@(xv, yv) (Second pts) = Second $ map (moveHilbert v) pts
  moveHilbert v@(xv, yv) (Nth pts) = Nth $ map (moveHilbert v) pts

  spaceOutGens :: Hilbert -> Hilbert
  spaceOutGens (Second pts) = Second $ zipWith moveHilbert [(0,0), (spacing, 0), (spacing, spacing), (0, spacing)] pts
  spaceOutGens (Nth pts) = Nth $ zipWith moveHilbert [(0,0), (spacing, 0), (spacing, spacing), (0, spacing)] pts
  spaceOutGens h = h

  spaceOutGens' :: Hilbert -> Hilbert
  spaceOutGens' (Second pts) = Second $ zipWith moveHilbert [(0,0), (spacing * 2, 0), (spacing * 2, spacing * 2), (0, spacing * 2)] pts
  spaceOutGens' (Nth pts) = Nth $ zipWith moveHilbert [(0,0), (spacing * 2, 0), (spacing * 2, spacing * 2), (0, spacing * 2)] pts
  spaceOutGens' h = h

  renderHilbert :: Hilbert -> Picture
  renderHilbert (First pts) = color white $ line pts
  renderHilbert (Second pts) = Pictures $ map renderHilbert pts
  renderHilbert (Nth pts) = Pictures $ map renderHilbert pts

  render :: Picture
  render = renderHilbert $ spaceOutGens (genHilbert 3)

  main :: IO ()
  main = display (InWindow "Hilbert Curve" (900, 900) (300, 300)) black render