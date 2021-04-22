{-# LANGUAGE BangPatterns #-}
module Main where

import           Criterion.Main
import           Data.Function
import           System.Random
import           Unsafe.Coerce

import qualified Data.BRAL          as B
import qualified Data.IntMap.Strict as I
import qualified Data.RAList        as R


main :: IO ()
main = defaultMain
    -- NOTE: there is a faster/better way to create a map using fromAscList
    -- but we want to bench cons-ing because that is what we are using in our machine.
    [ bgroup "create/100" [ bench "bral" $ whnf (extendB B.nil) 100
                           , bench "ral" $ whnf (extendR R.empty) 100
                           , bench "imap" $ whnf (extendI I.empty) 100
                           ]
    , bgroup "create/250" [ bench "bral" $ whnf (extendB B.nil) 250
                           , bench "ral" $ whnf (extendR R.empty) 250
                           , bench "imap" $ whnf (extendI I.empty) 250
                            ]


    , bgroup "query/front/100" [ bench "bral" $ whnf (queryFrontB 100) b100
                                , bench "ral" $ whnf (queryFrontR 100) r100
                                , bench "imap" $ whnf (queryFrontI 100) i100
                                ]
    , bgroup "query/front/250" [ bench "bral" $ whnf (queryFrontB 250) b250
                                , bench "ral" $ whnf (queryFrontR 250) r250
                                , bench "imap" $ whnf (queryFrontI 250) i250
                                ]

    , bgroup "query/back/100" [ bench "bral" $ whnf (queryBackB 100) b100
                               , bench "ral" $ whnf (queryBackR 100) r100
                               , bench "imap" $ whnf (queryBackI 100) i100
                               ]
    , bgroup "query/back/250" [ bench "bral" $ whnf (queryBackB 250) b250
                               , bench "ral" $ whnf (queryBackR 250) r250
                               , bench "imap" $ whnf (queryBackI 250) i250
                               ]


    , bgroup "query/rand/100" [ bench "bral" $ whnf (uncurry queryRandB) (rand100, b100)
                               , bench "ral" $ whnf (uncurry queryRandR) (rand100, r100)
                               , bench "imap" $ whnf (uncurry queryRandI) (rand100, i100)
                               ]
    , bgroup "query/rand/250" [ bench "bral" $ whnf (uncurry queryRandB) (rand250, b250)
                               , bench "ral" $ whnf (uncurry queryRandR) (rand250, r250)
                               , bench "imap" $ whnf (uncurry queryRandI) (rand250, i250)
                               ]

    , bgroup "create100/front100/cons100/back100/cons100/rand100"
            [ bench "bral" $ whnf (uncurry $ mixB 100 100 100 100) (rand100, b100)
            , bench "ral" $ whnf (uncurry $ mixR 100 100 100 100) (rand100, r100)
            , bench "imap" $ whnf (uncurry $ mixI 100 100 100 100) (rand100, i100)
            ]
    , bgroup "create250/front100/cons100/back100/cons100/rand250"
            [ bench "bral" $ whnf (uncurry $ mixB 100 100 100 100) (rand250, b250)
            , bench "ral" $ whnf (uncurry $ mixR 100 100 100 100) (rand250, r250)
            , bench "imap" $ whnf (uncurry $ mixI 100 100 100 100) (rand250, i250)
            ]


    ]
  where
        b100 = extendB B.nil 100
        r100 = extendR R.empty 100
        i100 = extendI I.empty 100
        b250 = extendB B.nil 250
        r250 = extendR R.empty 250
        i250 = extendI I.empty 250
        rand100 = take 100 $ randomRs (0,100-1) g
        rand250 = take 250 $ randomRs (0,250-1) g
        g = mkStdGen 59950

--- EXTEND

extendB :: B.BList () -> Word -> B.BList ()
extendB initB = fix $
    \ rec !n -> if n == 0
               then initB
               else B.cons () $ rec (n-1)


extendR :: R.RAList () -> Int -> R.RAList ()
extendR initR  = fix $
    \ rec !n -> if n == 0
               then initR
               else R.cons () $ rec (n-1)


extendI :: I.IntMap () -> Int -> I.IntMap ()
extendI initI  = fix $
    \ rec !n -> if n == 0
               then initI
               else let n' = n-1
                    in I.insert n' () $ rec n'

-- QUERY FRONT

queryFrontB :: Word -> B.BList () -> ()
queryFrontB 0 _ = ()
queryFrontB !i d = B.indexZero d i' `seq` queryFrontB i' d
  where i' = i-1

queryFrontR :: Int -> R.RAList () -> ()
queryFrontR 0 _ = ()
queryFrontR !i d = d R.! i' `seq` queryFrontR i' d
  where i' = i-1

queryFrontI :: Int -> I.IntMap () -> ()
queryFrontI 0 _ = ()
queryFrontI !i d = d I.! i' `seq` queryFrontI i' d
  where i' = i-1

-- QUERY BACK

queryBackB :: Word -> B.BList () -> ()
queryBackB size = go 0
 where
   go !i d | i == size = ()
           | otherwise = B.indexZero d i `seq` go (i+1) d

queryBackR :: Int -> R.RAList () -> ()
queryBackR size = go 0
 where
   go !i d | i == size = ()
           | otherwise = d R.! i `seq` go (i+1) d

queryBackI :: Int -> I.IntMap () -> ()
queryBackI size = go 0
 where
   go !i d | i == size = ()
           | otherwise = d I.! i `seq` go (i+1) d

-- QUERY RAND

queryRandB :: [Word] -> B.BList () -> ()
queryRandB [] _     = ()
queryRandB (i:is) d = B.indexZero d i `seq` queryRandB is d

queryRandR :: [Word] -> R.RAList () -> ()
queryRandR [] _     = ()
queryRandR (i:is) d = d R.! unsafeCoerce i `seq` queryRandR is d

queryRandI :: [Word] -> I.IntMap () -> ()
queryRandI [] _     = ()
queryRandI (i:is) d = d I.! unsafeCoerce i `seq` queryRandI is d

-- MIX

mixB :: Word -> Word -> Word -> Word -> [Word] -> B.BList () -> ()
mixB front cons1 back cons2 rand d =
    queryFrontB front d
    `seq`
    let d1 = extendB d cons1
    in queryBackB back d1
    `seq`
    let d2 = extendB d1 cons2
    in queryRandB rand d2


mixR :: Int -> Int -> Int -> Int -> [Word] -> R.RAList () -> ()
mixR front cons1 back cons2 rand d =
    queryFrontR front d
    `seq`
    let d1 = extendR d cons1
    in queryBackR back d1
    `seq`
    let d2 = extendR d1 cons2
    in queryRandR rand d2

mixI :: Int -> Int -> Int -> Int -> [Word] -> I.IntMap () -> ()
mixI front cons1 back cons2 rand d =
    queryFrontI front d
    `seq`
    let d1 = extendI d cons1
    in queryBackI back d1
    `seq`
    let d2 = extendI d1 cons2
    in queryRandI rand d2





