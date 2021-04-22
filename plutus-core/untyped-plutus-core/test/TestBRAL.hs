{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
import qualified Data.BRAL             as B
import           Data.Function
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

instance Arbitrary a => Arbitrary (B.BList a) where
    arbitrary = sized $ \n ->
        frequency
        [ (1, pure B.nil)
        , (n, B.cons <$> arbitrary <*> arbitrary)
        ]

hundred :: B.BList Word
hundred = foldr B.cons B.nil [1..100]

prop_null x = not $ B.null $ B.cons x B.nil

prop_cons1 x = B.cons x hundred /= hundred
prop_cons2 x xs = B.cons x xs /= xs


unit_null1 = not (B.null hundred) @? "list is empty"
unit_null2 = B.null B.nil @? "list not empty"

unit_head1 = B.head hundred @?= 1
unit_head2 = B.head (B.cons 0 hundred) @?= 0
unit_head3 = B.head (foldr B.cons hundred [200.. 300]) @?= 200
unit_head4 = (B.head (B.tail hundred) == B.head hundred + 1) @? "head mismatch"

unit_tail1 = B.head (B.tail (B.tail hundred)) == 3 @?  "tail broken"
unit_tail2 = B.tail (B.cons 1 hundred) == hundred @? "tail is not inverse of cons"
unit_tail3 = B.null (B.tail (B.cons () B.nil)) @? "tail is not inverse of cons"
unit_tail4 = (tailN 150 (applyN (B.cons 0) 100 hundred) == tailN 50 hundred) @? "tail/cons broken"
    where
      tailN :: Word -> B.BList a -> B.BList a
      tailN = applyN B.tail

applyN :: (a->a) -> Word -> a -> a
applyN f = fix $ \rec -> \case
                       0 -> id
                       n -> f . rec (n-1)

unit_ix1 = B.index hundred 1 == B.head hundred @? "index error"
unit_ix2 = B.index (B.cons 1 (B.cons 2 B.nil)) 2 @?= 2
unit_ix3 = all (\x -> B.index hundred x == x) [1..100] @? "index wrong"
prop_ix1 x y = B.index (B.cons x (B.cons y B.nil)) 1 == x
prop_ix2 x y = B.index (B.cons x (B.cons y B.nil)) 2 == y
prop_ix3 xs =
    not (B.null xs) ==>
    B.index xs 1 == B.head xs

unit_ixzero1 = B.indexZero hundred 0 == B.head hundred @? "index error"
unit_ixzero2 = B.indexZero (B.cons 1 (B.cons 2 B.nil)) 1 @?= 2
unit_ixzero3 = all (\x -> B.indexZero hundred (x-1) == x) [1..100] @? "indexZero wrong"
prop_ixzero1 x y = B.indexZero (B.cons x (B.cons y B.nil)) 0 == x
prop_ixzero2 x y = B.indexZero (B.cons x (B.cons y B.nil)) 1 == y
prop_ixzero3 xs =
    not (B.null xs) ==>
    B.indexZero xs 0 == B.head xs

prop_fail1 x = total $ B.index x 0
prop_fail2 (NonZero i) = total $ B.index (applyN (B.cons ()) i B.nil) (i+1)
prop_fail3 (NonZero i) = total $ B.indexZero (applyN (B.cons ()) i B.nil) i
prop_fail4 i = i > 1 ==> total $ B.index (B.nil :: B.BList ()) i
prop_fail5 (NonZero i) = total $ B.indexZero (B.nil :: B.BList ()) i

prop_constail :: Eq a => Word -> Word -> a -> B.BList a -> Bool
prop_constail reps skips x xs =
    applyN (applyN B.tail skips . applyN (B.cons x) skips) reps xs == xs

-- needed by QuickCheck TH
$(pure [])

main :: IO ()
main = defaultMain $ testGroup "BRAL"
    [ testProperty "prop_null" $(monomorphic 'prop_null)
    , testCase "unit_null1" unit_null1
    , testCase "unit_null2" unit_null2
    , testProperty "prop_cons1" $(monomorphic 'prop_cons1)
    , testProperty "prop_cons2" $(monomorphic 'prop_cons2)
    , testCase "unit_head1" unit_head1
    , testCase "unit_head2" unit_head2
    , testCase "unit_head3" unit_head3
    , testCase "unit_head4" unit_head4
    , testCase "unit_tail1" unit_tail1
    , testCase "unit_tail2" unit_tail2
    , testCase "unit_tail3" unit_tail3
    , testCase "unit_tail4" unit_tail4
    , testCase "unit_ix1" unit_ix1
    , testCase "unit_ix2" unit_ix2
    , testCase "unit_ix3" unit_ix3
    , testProperty "prop_ix1" $(monomorphic 'prop_ix1)
    , testProperty "prop_ix2" $(monomorphic 'prop_ix2)
    , testProperty "prop_ix3" $(monomorphic 'prop_ix3)
    , testCase "unit_ixzero1" unit_ixzero1
    , testCase "unit_ixzero2" unit_ixzero2
    , testCase "unit_ixzero3" unit_ixzero3
    , testProperty "prop_ixzero1" $(monomorphic 'prop_ixzero1)
    , testProperty "prop_ixzero2" $(monomorphic 'prop_ixzero2)
    , testProperty "prop_ixzero3" $(monomorphic 'prop_ixzero3)
    , testProperty "prop_fail1" (expectFailure $(monomorphic 'prop_fail1))
    , testProperty "prop_fail2" (expectFailure $(monomorphic 'prop_fail2))
    , testProperty "prop_fail3" (expectFailure $(monomorphic 'prop_fail3))
    , testProperty "prop_fail4" (expectFailure $(monomorphic 'prop_fail4))
    , testProperty "prop_fail5" (expectFailure $(monomorphic 'prop_fail5))
    , testProperty "prop_constail" $(monomorphic 'prop_constail)
    ]
