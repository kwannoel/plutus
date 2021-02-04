module Language.PlutusCore.Generators.NEAT.Type where

{-# FOREIGN AGDA2HS
{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE StandaloneDeriving        #-}

import           Control.Enumerable
import           Control.Monad.Except
import           Language.PlutusCore
import           Language.PlutusCore.Generators.NEAT.Common

#-}

open import Relation.Binary.PropositionalEquality
open import Haskell.Prelude hiding (m)
open import Language.PlutusCore.Generators.NEAT.Common

{-# FOREIGN AGDA2HS
newtype Neutral a = Neutral
  { unNeutral :: a
  }
#-}
-- * Enumeration

-- ** Enumerating types

data TypeBuiltinG : Set where
  TyByteStringG  : TypeBuiltinG
  TyIntegerG     : TypeBuiltinG
  TyStringG      : TypeBuiltinG
  TyBoolG        : TypeBuiltinG
  TyUnitG        : TypeBuiltinG
  TyCharG        : TypeBuiltinG 


{-# COMPILE AGDA2HS TypeBuiltinG deriving (Show, Eq, Ord) #-}

{-# FOREIGN AGDA2HS
deriveEnumerable ''TypeBuiltinG
#-}

-- NOTE: Unusually, the application case is annotated with a kind.
--       The reason is eagerness and efficiency. If we have the kind
--       information at the application site, we can check the two
--       subterms in parallel, while evaluating as little as possible.

variable
  n m o : Set

postulate Kind : Set → Set

data TypeG (n : Set) : Set where
  TyVarG : n → TypeG n
  TyFunG : TypeG n → TypeG n → TypeG n
  TyIFixG : TypeG n  → Kind ⊤ → TypeG n → TypeG n
  TyForallG : Kind ⊤ → TypeG (S n) → TypeG n
  TyBuiltinG : TypeBuiltinG → TypeG n
  TyLamG : TypeG (S n) → TypeG n
  TyAppG : TypeG n → TypeG n → Kind ⊤ → TypeG n

{-# COMPILE AGDA2HS TypeG deriving (Typeable, Eq, Ord, Show) #-}

{-# FOREIGN AGDA2HS
deriving instance Ord (Kind ())

deriveEnumerable ''Kind

deriveEnumerable ''TypeG

type ClosedTypeG = TypeG Z

instance Functor TypeG where
  fmap = ren
#-}

ext : (m → n) → S m → S n
ext _ FZ     = FZ
ext f (FS x) = FS (f x)

{-# COMPILE AGDA2HS ext #-}

ren : (m → n) → TypeG m → TypeG n
ren f (TyVarG x) = TyVarG (f x)
ren f (TyFunG ty1 ty2) = TyFunG (ren f ty1) (ren f ty2)
ren f (TyIFixG ty1 k ty2) = TyIFixG (ren f ty1) k (ren f ty2)
ren f (TyForallG k ty) = TyForallG k (ren (ext f) ty)
ren _ (TyBuiltinG b) = TyBuiltinG b
ren f (TyLamG ty) = TyLamG (ren (ext f) ty)
ren f (TyAppG ty1 ty2 k) = TyAppG (ren f ty1) (ren f ty2) k

{-# COMPILE AGDA2HS ren #-}

ext-cong : {ρ ρ' : m → n} → (∀ x → ρ x ≡ ρ' x) → ∀ x → ext ρ x ≡ ext ρ' x
ext-cong p FZ     = refl
ext-cong p (FS x) = cong FS (p x)

ren-cong : {ρ ρ' : m → n} → (∀ x → ρ x ≡ ρ' x) → ∀ t → ren ρ t ≡ ren ρ' t
ren-cong p (TyVarG x)          = cong TyVarG (p x)
ren-cong p (TyFunG ty1 ty2)    = cong₂ TyFunG (ren-cong p ty1) (ren-cong p ty2)
ren-cong p (TyIFixG ty1 k ty2) =
  cong₂ (λ ty1 ty2 → TyIFixG ty1 k ty2) (ren-cong p ty1) (ren-cong p ty2)
ren-cong p (TyForallG k ty)    = cong (TyForallG k) (ren-cong (ext-cong p) ty)
ren-cong p (TyBuiltinG b)      = refl
ren-cong p (TyLamG ty)         = cong TyLamG (ren-cong (ext-cong p) ty)
ren-cong p (TyAppG ty1 ty2 k)  =
  cong₂ (λ ty1 ty2 → TyAppG ty1 ty2 k) (ren-cong p ty1) (ren-cong p ty2)

-- ext (map for S) satisfies the functor laws

ext-id : (x : S m) → ext id x ≡ x
ext-id FZ     = refl
ext-id (FS x) = refl

ext-comp : (x : S m)(ρ : m → n)(ρ' : n → o) → ext (ρ' ∘ ρ) x ≡ ext ρ' (ext ρ x)
ext-comp FZ     ρ ρ' = refl
ext-comp (FS x) ρ ρ' = refl

-- ren (map for TypeG) satisfies the functor laws

ren-id : (ty : TypeG m) → ren id ty ≡ ty 
ren-id (TyVarG _)          = refl
ren-id (TyFunG ty1 ty2)    = cong₂ TyFunG (ren-id ty1) (ren-id ty2)
ren-id (TyIFixG ty1 k ty2) =
  cong₂ (λ ty1 ty2 → TyIFixG ty1 k ty2) (ren-id ty1) (ren-id ty2)
ren-id (TyForallG k ty)    =
  cong (TyForallG k) (trans (ren-cong ext-id ty) (ren-id ty))
ren-id (TyBuiltinG _)      = refl
ren-id (TyLamG ty)         =
  cong TyLamG (trans (ren-cong ext-id ty) (ren-id ty))
ren-id (TyAppG ty1 ty2 k)  =
  cong₂ (λ ty1 ty2 → TyAppG ty1 ty2 k) (ren-id ty1) (ren-id ty2)

ren-comp : (ty : TypeG m)(ρ : m → n)(ρ' : n → o)
         → ren (ρ' ∘ ρ) ty ≡ ren ρ' (ren ρ ty)
ren-comp (TyVarG x)          ρ ρ' = refl
ren-comp (TyFunG ty1 ty2)    ρ ρ' =
  cong₂ TyFunG (ren-comp ty1 ρ ρ') (ren-comp ty2 ρ ρ')
ren-comp (TyIFixG ty1 k ty2) ρ ρ' =
  cong₂ (λ ty1 ty2 → TyIFixG ty1 k ty2) (ren-comp ty1 ρ ρ') (ren-comp ty2 ρ ρ')
ren-comp (TyForallG k ty)    ρ ρ' = cong
  (TyForallG k)
  (trans (ren-cong (λ x → ext-comp x ρ ρ') ty)
         (ren-comp ty (ext ρ) (ext ρ')))
ren-comp (TyBuiltinG b)      ρ ρ' = refl
ren-comp (TyLamG ty)         ρ ρ' = cong
  TyLamG
  (trans (ren-cong (λ x → ext-comp x ρ ρ') ty)
         (ren-comp ty (ext ρ) (ext ρ')))
ren-comp (TyAppG ty1 ty2 k)  ρ ρ' =
  cong₂ (λ ty1 ty2 → TyAppG ty1 ty2 k) (ren-comp ty1 ρ ρ') (ren-comp ty2 ρ ρ')


-- ** Type reduction

-- |Extend type substitutions.
exts : (n → TypeG m) -> S n → TypeG (S m)
exts _ FZ     = TyVarG FZ
exts s (FS i) = ren FS (s i) -- FS <$> s i

{-# COMPILE AGDA2HS exts #-}

-- |Simultaneous substitution of type variables.
sub : (n -> TypeG m) -> TypeG n -> TypeG m
sub s (TyVarG i)             = s i
sub s (TyFunG ty1 ty2)       =
  TyFunG (sub s ty1) (sub s ty2)
sub s (TyIFixG ty1 k ty2)    =
  TyIFixG (sub s ty1) k (sub s ty2)
sub s (TyForallG k ty)       = TyForallG k (sub (exts s) ty)
sub _ (TyBuiltinG tyBuiltin) = TyBuiltinG tyBuiltin
sub s (TyLamG ty)            = TyLamG (sub (exts s) ty)
sub s (TyAppG ty1 ty2 k)     =
  TyAppG (sub s ty1) (sub s ty2) k

{-# COMPILE AGDA2HS sub #-}

{-# FOREIGN AGDA2HS
instance Monad TypeG where
  a >>= f = sub f a
--  return = pure

instance Applicative TypeG where
  (<*>) = ap
  pure = TyVarG
#-}

-- sub ((=<<) for TypeG) satisfies the monad laws

exts-cong : {σ σ' : m → TypeG n} → (∀ x → σ x ≡ σ' x)
          → ∀ x → exts σ x ≡ exts σ' x
exts-cong p FZ     = refl
exts-cong p (FS x) = cong (ren FS) (p x)

sub-cong : {σ σ' : m → TypeG n} → (∀ x → σ x ≡ σ' x)
         → ∀ ty → sub σ ty ≡ sub σ' ty
sub-cong p (TyVarG x)          = p x
sub-cong p (TyFunG ty1 ty2)    = cong₂ TyFunG (sub-cong p ty1) (sub-cong p ty2)
sub-cong p (TyIFixG ty1 k ty2) =
  cong₂ (λ ty1 ty2 → TyIFixG ty1 k ty2) (sub-cong p ty1) (sub-cong p ty2)
sub-cong p (TyForallG k ty)    = cong (TyForallG k) (sub-cong (exts-cong p) ty)
sub-cong p (TyBuiltinG b)      = refl
sub-cong p (TyLamG ty)         = cong TyLamG (sub-cong (exts-cong p) ty)
sub-cong p (TyAppG ty1 ty2 k)  =
  cong₂ (λ ty1 ty2 → TyAppG ty1 ty2 k) (sub-cong p ty1) (sub-cong p ty2)

exts-id : (x : S m) → exts TyVarG x ≡ TyVarG x
exts-id FZ     = refl
exts-id (FS x) = refl

sub-id : (t : TypeG m) → sub TyVarG t ≡ t
sub-id (TyVarG x)          = refl
sub-id (TyFunG ty1 ty2)    = cong₂ TyFunG (sub-id ty1) (sub-id ty2)
sub-id (TyIFixG ty1 k ty2) =
  cong₂ (λ ty1 ty2 → TyIFixG ty1 k ty2) (sub-id ty1) (sub-id ty2)
sub-id (TyForallG k ty)    = cong
  (TyForallG k)
  (trans (sub-cong exts-id ty) (sub-id ty))
sub-id (TyBuiltinG b)      = refl
sub-id (TyLamG ty)         = cong
  TyLamG
  (trans (sub-cong exts-id ty) (sub-id ty))
sub-id (TyAppG ty1 ty2 k)  = cong₂
  (λ ty1 ty2 → TyAppG ty1 ty2 k)
  (sub-id ty1)
  (sub-id ty2)

exts-ext : (x : S m)(ρ : m → n)(σ : n → TypeG o)
         → exts (σ ∘ ρ) x ≡ exts σ (ext ρ x)
exts-ext FZ     σ ρ = refl
exts-ext (FS x) σ ρ = refl

sub-ren : (t : TypeG m)(ρ : m → n)(σ : n → TypeG o)
         → sub (σ ∘ ρ) t ≡ sub σ (ren ρ t)
sub-ren (TyVarG x)          ρ σ = refl
sub-ren (TyFunG ty1 ty2)    ρ σ =
  cong₂ TyFunG (sub-ren ty1 ρ σ) (sub-ren ty2 ρ σ)
sub-ren (TyIFixG ty1 k ty2) ρ σ =
  cong₂ (λ ty1 ty2 → TyIFixG ty1 k ty2) (sub-ren ty1 ρ σ) (sub-ren ty2 ρ σ)

sub-ren (TyForallG k ty)    ρ σ = cong
  (TyForallG k)
  (trans (sub-cong (λ x → exts-ext x ρ σ) ty)
         (sub-ren ty (ext ρ) (exts σ)))
sub-ren (TyBuiltinG b)      ρ σ = refl
sub-ren (TyLamG ty)         ρ σ = cong
  TyLamG
  (trans (sub-cong (λ x → exts-ext x ρ σ) ty)
         (sub-ren ty (ext ρ) (exts σ)))
sub-ren (TyAppG ty1 ty2 k)  ρ σ =
  cong₂ (λ ty1 ty2 → TyAppG ty1 ty2 k) (sub-ren ty1 ρ σ) (sub-ren ty2 ρ σ)

ext-exts : (x : S m)(σ : m → TypeG n)(ρ : n → o)
         → exts (ren ρ ∘ σ) x ≡ ren (ext ρ) (exts σ x)
ext-exts FZ     σ ρ = refl
ext-exts (FS x) σ ρ = trans
  (sym (ren-comp (σ x) ρ FS))
  (ren-comp (σ x) FS (ext ρ))

ren-sub : (t : TypeG m)(σ : m → TypeG n)(ρ : n → o)
         → sub (ren ρ ∘ σ) t ≡ ren ρ (sub σ t)
ren-sub (TyVarG x)          σ ρ = refl
ren-sub (TyFunG ty1 ty2)    σ ρ =
  cong₂ TyFunG (ren-sub ty1 σ ρ) (ren-sub ty2 σ ρ)
ren-sub (TyIFixG ty1 k ty2) σ ρ =
  cong₂ (λ ty1 ty2 → TyIFixG ty1 k ty2) (ren-sub ty1 σ ρ) (ren-sub ty2 σ ρ)
ren-sub (TyForallG k ty)    σ ρ = cong
  (TyForallG k)
  (trans (sub-cong (λ x → ext-exts x σ ρ) ty)
         (ren-sub ty (exts σ) (ext ρ)))
ren-sub (TyBuiltinG b)      σ ρ = refl
ren-sub (TyLamG ty)         σ ρ = cong
  TyLamG
  (trans (sub-cong (λ x → ext-exts x σ ρ) ty)
         (ren-sub ty (exts σ) (ext ρ)))
ren-sub (TyAppG ty1 ty2 k)  σ ρ =
  cong₂ (λ ty1 ty2 → TyAppG ty1 ty2 k) (ren-sub ty1 σ ρ) (ren-sub ty2 σ ρ)

exts-comp : (x : S m)(σ : m → TypeG n)(σ' : n → TypeG o)
         → exts (sub σ' ∘ σ) x ≡ sub (exts σ') (exts σ x)
exts-comp FZ     σ σ' = refl
exts-comp (FS x) σ σ' = trans
  (sym (ren-sub (σ x) σ' FS))
  (sub-ren (σ x) FS (exts σ'))

sub-comp : (ty : TypeG m)(σ : m → TypeG n)(σ' : n → TypeG o)
         → sub (sub σ' ∘ σ) ty ≡ sub σ' (sub σ ty)
sub-comp (TyVarG x)          σ σ' = refl
sub-comp (TyFunG ty1 ty2)    σ σ' =
  cong₂ TyFunG (sub-comp ty1 σ σ') (sub-comp ty2 σ σ')
sub-comp (TyIFixG ty1 k ty2) σ σ' =
  cong₂ (λ ty1 ty2 → TyIFixG ty1 k ty2) (sub-comp ty1 σ σ') (sub-comp ty2 σ σ')
sub-comp (TyForallG k ty)    σ σ' = cong
  (TyForallG k)
  (trans (sub-cong (λ x → exts-comp x σ σ') ty)
         (sub-comp ty (exts σ) (exts σ')))
sub-comp (TyBuiltinG b)      σ σ' = refl
sub-comp (TyLamG ty)         σ σ' = cong
  TyLamG
  (trans (sub-cong (λ x → exts-comp x σ σ') ty)
         (sub-comp ty (exts σ) (exts σ')))
sub-comp (TyAppG ty1 ty2 k)  σ σ' =
  cong₂ (λ ty1 ty2 → TyAppG ty1 ty2 k) (sub-comp ty1 σ σ') (sub-comp ty2 σ σ')
