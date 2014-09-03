
import Data.BinaryList (BinList)
import qualified Data.BinaryList as BL

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (fromJust)
import qualified Data.Foldable as F
import Control.Arrow ((&&&))

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- Theory part

data Bijection a b = Bijection { direct :: a -> b , inverse :: b -> a }

binaryTransform :: Bijection (a,a) (a,a) -> Bijection (BinList a) (BinList a)
binaryTransform (Bijection f g) = Bijection transform itransform
   where
     transform xs =
       case BL.disjoinPairs xs of
         Nothing -> xs
         Just ps -> let (l,r) = BL.unzip $ fmap f ps
                    in  fromJust $ BL.append (transform l) r
     itransform xs =
       case BL.split xs of
         Left _ -> xs
         Right (l,r) -> BL.joinPairs $ fmap g $ BL.zip (itransform l) r

partialInverse :: Bijection (BinList a) (BinList a) -> Int -> BinList a -> BinList a
partialInverse (Bijection _ itransform) = go
  where
    go 0 xs = itransform xs
    go n xs =
      case BL.split xs of
        Right (l,_) -> go (n-1) l
        _ -> xs

---------------------------------------------------------------------------------
-- Data part

theExp :: Int
theExp = 11

nextExp :: Int -> Int
nextExp i = min (i+1) (theExp-1)

previousExp :: Int -> Int
previousExp i = max (i-1) 0

theSize :: Int
theSize = 2 ^ theExp

scaling :: Float
scaling = 50

outSpeed :: Float
outSpeed = 1/800

inSpeed :: Float
inSpeed = 1/1200

tau :: Float
tau = 2*pi

theFunction :: Float -> Float
theFunction x = scaling * sin (tau * outSpeed * x * sin (tau * inSpeed * x)) + 1

theData :: BinList Float
theData = fromJust $ BL.fromList [ theFunction $ fromIntegral i | i <- [1 .. theSize] ]

{-
theData :: BinList Float
theData = fromJust . BL.fromList.(fmap $ (* scaling).fromIntegral) . concat . replicate 256 $ lst
 where
   lst :: [Integer]
   lst = [0,0,2,1,0,0,0,0]
-} 

data Option = Identity | Minimum | Average | Average2 | Linear deriving (Eq,Ord,Show)

instance Enum Option where
  succ Identity = Minimum
  succ Minimum  = Average
  succ Average  = Average2
  succ Average2 = Linear
  succ Linear   = Linear
  pred Identity = Identity
  pred Minimum  = Identity
  pred Average  = Minimum
  pred Average2 = Average
  pred Linear   = Average2

  toEnum i
    | i <= 0 = Identity
    | i == 1 = Minimum
    | i == 2 = Average
    | i == 3 = Average2
    | otherwise = Linear

  fromEnum Identity = 0
  fromEnum Minimum  = 1
  fromEnum Average  = 2
  fromEnum Average2 = 3
  fromEnum Linear   = 4

allOptions :: [Option]
allOptions = [Identity .. Linear]

maxfun :: (Float,Float) -> Float
maxfun (x,y) = (x + y + abs (x-y)) / 2

minfun :: (Float,Float) -> Float
minfun (x,y) = (x + y - abs (x-y)) / 2

-- | Given a number @p@ from 0 to 1, 'olinearBijection' builds a linear isomorphism @f@
--   with matrix
--
-- > ( 1-p    p )
-- > (  -p  1-p )
--
--   This linear isomorphism enjoy two properties:
--
--   * Both rows (columns) are orthogonal (orthonormal if @p@ is either one or zero).
--
--   * @(fst . f) (1,0) + (fst . f) (0,1) = 1@
--
olinearBijection :: Float -> Bijection (Float,Float) (Float,Float)
olinearBijection p =
  let q = 1 - p
      d = p^2 + q^2
      pd = p / d
      qd = q / d
  in  Bijection
        (\(x,y) -> ( q  * x + p  * y , (-p) * x + q  * y ) )
        (\(x,y) -> ( qd * x - pd * y ,   pd * x + qd * y ) )

toBijection :: Option -> Bijection (Float,Float) (Float,Float)
toBijection Identity = Bijection id id
toBijection Minimum = Bijection (minfun &&& maxfun) (maxfun &&& minfun)
toBijection Average =
  Bijection
    (\(x,y) -> ((x+y)/2,(x-y)/2))
    (\(x,y) -> (x+y,x-y))
toBijection Average2 =
  Bijection
    (\(x,y) -> ((x+y)/2,(x-y)/4))
    (\(x,y) -> (x+2*y,x-2*y))
toBijection Linear = olinearBijection (1/4)

allTransforms :: Map Option (Bijection (BinList Float) (BinList Float))
allTransforms = M.fromList $ fmap (\o -> (o,binaryTransform $ toBijection o)) allOptions

encodedData :: Map Option (BinList Float)
encodedData = M.fromList $ fmap (\o -> (o,direct (allTransforms M.! o) $ theData)) allOptions

decodedData :: Map Option (Map Int (BinList Float))
decodedData = M.fromList $
  fmap (\o -> (o,M.fromList $
                  fmap (\i -> (i,partialInverse (allTransforms M.! o) i (encodedData M.! o))
                         ) [0..theExp]
                )
         ) allOptions

-- Gloss part

data World = World
  { currentOption :: Option
  , currentLevel  :: Int
    }

width :: Float
width = 1400

height :: Float
height = 600

xscale :: Int -> Float
xscale l = width * recip (fromIntegral (l - 1))

coordinate :: Int -> Int -> Float -> (Float,Float)
coordinate l i x =
  ( fromIntegral i * xscale l - width/2
  , x
    )

drawBinList :: BinList Float -> Picture
drawBinList xs = g $ F.foldl f ([],0) xs
  where
    l = BL.length xs
    f (ps,i) x = let p = coordinate l i x in (p : ps , i+1)
    g (ps,_) = Pictures $ Line ps : fmap (\(x,y) -> Translate x y $ circleSolid 4) ps

worldPicture :: World -> Picture
worldPicture w = Color white $ Pictures
  [ drawBinList $ (decodedData M.! currentOption w) M.! currentLevel w
  , Translate (negate $ 4/5*width/2) (4/5*height/2) $ Scale (1/6) (1/6) $ Text $ "Current Method: " ++ show (currentOption w)
  , Translate (         4/5*width/4) (4/5*height/2) $ Scale (1/6) (1/6) $ Text $ "Resolution: 1/" ++ show (2^currentLevel w)
    ]

worldEvent :: Event -> World -> World
worldEvent ev w =
  case ev of
    EventKey (SpecialKey k) Down _ _ ->
      case k of
        KeyUp    -> w { currentOption = succ (currentOption w) }
        KeyDown  -> w { currentOption = pred (currentOption w) }
        KeyLeft  -> w { currentLevel  = previousExp (currentLevel  w) }
        KeyRight -> w { currentLevel  = nextExp     (currentLevel  w) }
        _ -> w
    _ -> w

main :: IO ()
main =
  play (InWindow "Binary Transform Display" (round width, round height) (0,0))
       (makeColor8 0 0 0 0)
        1 -- Steps per second
       (World Identity 0)
        worldPicture
        worldEvent
       (const id)
