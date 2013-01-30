{-# LANGUAGE CPP, GADTs, TypeOperators #-}

module Main
( main
) where

--------------------------------------------------------------------------------
import           Prelude hiding (lookup)
--------------------------------------------------------------------------------
import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad.Trans (liftIO)
--------------------------------------------------------------------------------
import           Criterion.Config
import           Criterion.Main
--------------------------------------------------------------------------------
import           Data.Bits             ((.&.))
import qualified Data.ByteString as BS
import qualified Data.IntMap     as IM
import qualified Data.Map        as M
import           Data.List             (foldl')
import           Data.Maybe            (fromMaybe)
import           Data.Proxy
--------------------------------------------------------------------------------
import qualified Data.Store as DS
import qualified Data.Store.Key as DS
import           Data.Store.Key ((.:), (:.)(..))
import           Data.Store.Query.Selection
--------------------------------------------------------------------------------
import qualified Util.ByteString as UBS
import qualified Util.Int        as UI
import qualified Util.String     as US
--------------------------------------------------------------------------------

data B where
    B :: NFData a => a -> B

instance NFData B where
    rnf (B b) = rnf b

main :: IO ()
main = do
    let mS  = M.fromList elems   :: M.Map String Int
        mBS = M.fromList elemsBS :: M.Map BS.ByteString Int
        mI  = M.fromList elemsI  :: M.Map Int Int
        dsS  = DS.fromList (map (\(k, v) -> (dsKey k, v)) elems)   :: DataStore String
        dsBS = DS.fromList (map (\(k, v) -> (dsKey k, v)) elemsBS) :: DataStore BS.ByteString
        dsI  = DS.fromList (map (\(k, v) -> (dsKey k, v)) elemsI)  :: DataStore Int
        im = IM.fromList elemsI :: IM.IntMap Int
    defaultMainWith defaultConfig
        (liftIO . evaluate $ rnf [B mS, B mBS, B mI, B im])
        [ bgroup "lookup"
          [ bgroup "String"
            [ bench "Map"   $ whnf (lookupM  keys)  mS
            , bench "Store" $ whnf (lookupDS keys) dsS
            ]
          , bgroup "ByteString"
            [ bench "Map"   $ whnf (lookupM  keysBS)  mBS
            , bench "Store" $ whnf (lookupDS keysBS) dsBS
            ]
          , bgroup "Int"
            [ bench "Map"    $ whnf (lookupM keysI)  mI
            , bench "IntMap" $ whnf (lookupIM keysI) im
            , bench "Store"  $ whnf (lookupDS keysI) dsI
            ]
          ]
        , bgroup "lookup-miss"
          [ bgroup "String"
            [ bench "Map"   $ whnf (lookupM  keys')  mS
            , bench "Store" $ whnf (lookupDS keys') dsS
            ]
          , bgroup "ByteString"
            [ bench "Map"   $ whnf (lookupM  keysBS')  mBS
            , bench "Store" $ whnf (lookupDS keysBS') dsBS
            ]
          , bgroup "Int"
            [ bench "Map"    $ whnf (lookupM keysI')  mI
            , bench "IntMap" $ whnf (lookupIM keysI') im
            , bench "Store"  $ whnf (lookupDS keysI') dsI
            ]
          ]
        , bgroup "insert"
          [ bgroup "String"
            [ bench "Map"   $ whnf (insertM  elems)  M.empty
            , bench "Store" $ whnf (insertDS elems) DS.empty
            ]
          , bgroup "ByteString"
            [ bench "Map"   $ whnf (insertM  elemsBS)  M.empty
            , bench "Store" $ whnf (insertDS elemsBS) DS.empty
            ]
          , bgroup "Int"
            [ bench "Map"    $ whnf (insertM  elemsI)  M.empty
            , bench "IntMap" $ whnf (insertIM elemsI) IM.empty
            , bench "Store"  $ whnf (insertDS elemsI) DS.empty
            ]
          ]
        , bgroup "insert-duplicate"
          [ bgroup "String"
            [ bench "Map" $ whnf (insertM elems) mS
            , bench "Store" $ whnf (insertDS elems) dsS
            ]
          , bgroup "ByteString"
            [ bench "Map" $ whnf (insertM elemsBS) mBS
            , bench "Store" $ whnf (insertDS elemsBS) dsBS
            ]
          , bgroup "Int"
            [ bench "Map"    $ whnf (insertM elemsI)  mI
            , bench "IntMap" $ whnf (insertIM elemsI) im
            , bench "Store"  $ whnf (insertDS elemsI) dsI
            ]
          ]
        , bgroup "delete"
          [ bgroup "String"
            [ bench "Map" $ whnf (deleteM keys) mS
            , bench "Store" $ whnf (deleteDS keys) dsS
            ]
          , bgroup "ByteString"
            [ bench "Map" $ whnf (deleteM keysBS) mBS
            , bench "Store" $ whnf (deleteDS keysBS) dsBS
            ]
          , bgroup "Int"
            [ bench "Map"    $ whnf (deleteM keysI)  mI
            , bench "IntMap" $ whnf (deleteIM keysI) im
            , bench "Store" $ whnf (deleteDS keysI) dsI
            ]
          ]
        , bgroup "delete-miss"
          [ bgroup "String"
            [ bench "Map" $ whnf (deleteM keys') mS
            , bench "Store" $ whnf (deleteDS keys') dsS
            ]
          , bgroup "ByteString"
            [ bench "Map" $ whnf (deleteM keysBS') mBS
            , bench "Store" $ whnf (deleteDS keysBS') dsBS
            ]
          , bgroup "Int"
            [ bench "Map"    $ whnf (deleteM keysI')  mI
            , bench "IntMap" $ whnf (deleteIM keysI') im
            , bench "Store" $ whnf (deleteDS keysI') dsI
            ]
          ]
        , bgroup "fromList"
          [ bgroup "String"
            [ bench "Map" $ whnf M.fromList elems
            , bench "Store" $ whnf DS.fromList $! map (\(k, v) -> (dsKey k, v)) elems
            ]
          , bgroup "ByteString"
            [ bench "Map" $ whnf M.fromList elemsBS
            , bench "Store" $ whnf DS.fromList $! map (\(k, v) -> (dsKey k, v)) elemsBS
            ]
          , bgroup "Int"
            [ bench "Map"    $ whnf M.fromList elemsI
            , bench "IntMap" $ whnf IM.fromList elemsI
            , bench "Store" $ whnf DS.fromList $! map (\(k, v) -> (dsKey k, v)) elemsI
            ]
          ]
        , bgroup "size"
          [ bgroup "String"
            [ bench "Map" $ whnf M.size mS
            , bench "Store" $ whnf DS.size dsS
            ]
          , bgroup "ByteString"
            [ bench "Map" $ whnf M.size mBS
            , bench "Store" $ whnf DS.size dsBS
            ]
          , bgroup "Int"
            [ bench "Map"    $ whnf M.size  mI
            , bench "IntMap" $ whnf IM.size im
            , bench "Store" $ whnf DS.size dsI
            ]
          ]
        ]
  where
    n :: Int
    n = 2^(16 :: Int)

    elems   = zip keys [1..n]
    keys    = US.rnd 8 n
    elemsBS = zip keysBS [1..n]
    keysBS  = UBS.rnd 8 n
    elemsI  = zip keysI [1..n]
    keysI   = UI.rnd (n+n) n
    elemsI2 = zip [n `div` 2..n + (n `div` 2)] [1..n]  -- for union

    keys'    = US.rnd' 8 n
    keysBS'  = UBS.rnd' 8 n
    keysI'   = UI.rnd' (n+n) n

    keysDup    = US.rnd 2 n
    keysDupBS  = UBS.rnd 2 n
    keysDupI   = UI.rnd (n`div`4) n
    elemsDup   = zip keysDup [1..n]
    elemsDupBS = zip keysDupBS [1..n]
    elemsDupI  = zip keysDupI [1..n]

------------------------------------------------------------------------
-- * Data Store

data DSTag = DSTag
type DataStore k = DS.Store DSTag ((k, DS.Dim) :. DS.K0) Int

sFirst :: Proxy (DSTag, DS.N0)
sFirst = Proxy

dsKey :: Ord k => k -> DS.Key ((k, DS.Dim) :. DS.K0)
dsKey x = DS.K1 $! DS.dimN [x]
{-# INLINEABLE dsKey #-}
{-# SPECIALIZE dsKey :: String -> DS.Key ((String, DS.Dim) :. DS.K0) #-}
{-# SPECIALIZE dsKey :: BS.ByteString -> DS.Key ((BS.ByteString, DS.Dim) :. DS.K0) #-}
{-# SPECIALIZE dsKey :: Int -> DS.Key ((Int, DS.Dim) :. DS.K0) #-}

lookupDS :: Ord k => [k] -> DataStore k -> Int
lookupDS xs ds = foldl' (\z k -> safeHead z (DS.lookup' (sFirst .== k) ds)) 0 xs
    where
      safeHead _ (x:_) = x
      safeHead d _ = d
      {-# INLINEABLE safeHead #-}
{-# SPECIALIZE lookupDS :: [String] -> DataStore String -> Int #-}
{-# SPECIALIZE lookupDS :: [BS.ByteString] -> DataStore BS.ByteString -> Int #-}
{-# SPECIALIZE lookupDS :: [Int] -> DataStore Int -> Int #-}

insertDS :: Ord k => [(k, Int)] -> DataStore k -> DataStore k
insertDS xs m0 = foldl' (\ds (k, v) -> DS.insert' (dsKey k) v ds) m0 xs
{-# SPECIALIZE insertDS :: [(String, Int)] -> DataStore String
                        -> DataStore String #-}
{-# SPECIALIZE insertDS :: [(BS.ByteString, Int)] -> DataStore BS.ByteString
                        -> DataStore BS.ByteString #-}
{-# SPECIALIZE insertDS :: [(Int, Int)] -> DataStore Int
                        -> DataStore Int #-}

deleteDS :: Ord k=> [k] -> DataStore k -> DataStore k
deleteDS xs m0 = foldl' (\ds k -> DS.update (const Nothing) (sFirst .== k) ds) m0 xs
{-# SPECIALIZE deleteDS :: [String] -> DataStore String
                        -> DataStore String #-}
{-# SPECIALIZE deleteDS :: [BS.ByteString] -> DataStore BS.ByteString
                        -> DataStore BS.ByteString #-}
{-# SPECIALIZE deleteDS :: [Int] -> DataStore Int
                        -> DataStore Int #-}

------------------------------------------------------------------------
-- * Map

lookupM :: Ord k => [k] -> M.Map k Int -> Int
lookupM xs m = foldl' (\z k -> fromMaybe z (M.lookup k m)) 0 xs
{-# SPECIALIZE lookupM :: [String] -> M.Map String Int -> Int #-}
{-# SPECIALIZE lookupM :: [BS.ByteString] -> M.Map BS.ByteString Int -> Int #-}
{-# SPECIALIZE lookupM :: [Int] -> M.Map Int Int -> Int #-}

insertM :: Ord k => [(k, Int)] -> M.Map k Int -> M.Map k Int
insertM xs m0 = foldl' (\m (k, v) -> M.insert k v m) m0 xs
{-# SPECIALIZE insertM :: [(String, Int)] -> M.Map String Int
                       -> M.Map String Int #-}
{-# SPECIALIZE insertM :: [(BS.ByteString, Int)] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}
{-# SPECIALIZE insertM :: [(Int, Int)] -> M.Map Int Int
                       -> M.Map Int Int #-}

deleteM :: Ord k => [k] -> M.Map k Int -> M.Map k Int
deleteM xs m0 = foldl' (\m k -> M.delete k m) m0 xs
{-# SPECIALIZE deleteM :: [String] -> M.Map String Int -> M.Map String Int #-}
{-# SPECIALIZE deleteM :: [BS.ByteString] -> M.Map BS.ByteString Int
                       -> M.Map BS.ByteString Int #-}
{-# SPECIALIZE deleteM :: [Int] -> M.Map Int Int
                       -> M.Map Int Int #-}

------------------------------------------------------------------------
-- * IntMap

lookupIM :: [Int] -> IM.IntMap Int -> Int
lookupIM xs m = foldl' (\z k -> fromMaybe z (IM.lookup k m)) 0 xs

insertIM :: [(Int, Int)] -> IM.IntMap Int -> IM.IntMap Int
insertIM xs m0 = foldl' (\m (k, v) -> IM.insert k v m) m0 xs

deleteIM :: [Int] -> IM.IntMap Int -> IM.IntMap Int
deleteIM xs m0 = foldl' (\m k -> IM.delete k m) m0 xs
