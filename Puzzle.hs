{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GADTs  #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction, PatternGuards #-}
{-# LANGUAGE RankNTypes, TemplateHaskell, TupleSections, TypeFamilies        #-}
module Puzzle (Direction(..), Board, GameState(..), toLists, fromLists, newBlock
              , shift, blanks, randomPlace, newBoard, board, score, withIndex
              ) where
import Control.Applicative   ((<$>))
import Control.Arrow         (second)
import Control.Eff           (Eff, Member)
import Control.Eff.Exception (Exc, runExc, throwExc)
import Control.Eff.Random    hiding (next)
import Control.Lens          (Index, IxValue, Ixed (..), makeLenses, (&), (.~))
import Control.Monad         (liftM, when)
import Control.Monad.Writer  (Writer, runWriter, tell)
import Data.List             (transpose)
import Data.Maybe            (catMaybes, isNothing)
import Data.Monoid           (Sum (..))
import Data.Typeable

newtype Board =
  Board { _getBoard :: [[Maybe Int]]
        } deriving (Eq, Read, Ord, Typeable)

toLists :: Board -> [[Maybe Int]]
toLists = _getBoard

instance Show Board where
  show = init . unlines . map (unwords . map (maybe "_" show)) . toLists

data GameState = GS { _board :: Board
                    , _score :: Int
                    } deriving (Read, Show, Eq, Ord, Typeable)

makeLenses ''GameState

newBoard :: (Member Rand r) => Eff r Board
newBoard = fromRightU <$> runExc (randomPlace =<< randomPlace (fromLists []))

fromRightU :: Either () t -> t
fromRightU (Right a) = a
fromRightU (Left ()) = error "fromRightU"

randomPlace :: (Member (Exc ()) r, Member Rand r) => Board -> Eff r Board
randomPlace b = do
  let xs = blanks b
  when (null xs) $ throwExc ()
  idx <- fromList $ map (,1) xs
  new <- newBlock
  return $ b & ix idx .~ Just new

newBlock :: (Num a, Member Rand r) => Eff r a
newBlock = fromList [(2, 0.9), (4, 0.1)]

fromLists :: [[Maybe Int]] -> Board
fromLists rss =
  Board $ take 4 $
  map (take 4 . (++ repeat Nothing)) rss ++ repeat (replicate 4 Nothing)

data Direction = LeftD | RightD | UpD | DownD
               deriving (Read, Show, Eq, Ord, Enum)


shift' :: [Maybe Int] -> Writer (Sum Int) [Maybe Int]
shift' xs = do
  ans <- step $ catMaybes xs
  return $ take 4 $ map Just ans ++ repeat Nothing
  where
    step [] = return []
    step [a] = return [a]
    step (a : b : bs)
      | a == b    = tell (Sum $ 2 * a) >> (2 * a :) <$> step bs
      | otherwise = (a :) <$> step (b:bs)

shift :: Direction -> Board -> (Board, Int)
shift dir b = second getSum $ runWriter $ do
  fromLists . modifier <$> mapM (liftM rev . shift' . rev) (modifier $ toLists b)
  where
    (rev, modifier)
      | LeftD  <- dir = (id, id)
      | RightD <- dir = (reverse, id)
      | UpD    <- dir = (id, transpose)
      | otherwise     = (reverse, transpose)

blanks :: Board -> [(Int, Int)]
blanks  =
  map fst . filter (isNothing . snd) . withIndex

withIndex :: Board -> [((Int,Int), Maybe Int)]
withIndex (Board rbs) = concat $ zipWith (flip zipWith [0..] . ((,) .) . (,)) [0..] $ rbs

type instance IxValue Board = Maybe Int
type instance Index Board   = (Int, Int)

instance Ixed Board where
  ix (i, j) f (Board rbs) = Board <$> (ix i . ix j) f rbs
