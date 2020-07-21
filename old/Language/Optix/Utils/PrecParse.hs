-- Translated from mlton source code
-- https://github.com/MLton/mlton/blob/678049ecc3204247fe9a2583c66006eab57b84ce/mlton/elaborate/precedence-parse.fun#L136

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Optix.Utils.PrecParse 
    ( Fixval (..)
    , PrecError (..)
    , PrecParse (..)
    , parse
    ) where

-- | Whether a value is infix or not
--
-- In Haskell:
--   infix  n ~ Infix (n+n)   (n+n)
--   infixl n ~ Infix (n+n)   (n+n+1)
--   infixr n ~ Infix (n+n+1) (n+n)
data Fixval
    = Nonfix
    | Infix !Int !Int -- ^ Determines left/right precedence
    deriving (Show, Eq, Ord, Read)

-- | Possible errors that can occur
data PrecError a
    = StartsWithInfix !a    -- ^ The item list begins with an infix operator
    | EndsWithInfix !a      -- ^ The item list ends with an infix operator
    | InfixUsedNonfix !a    -- ^ The item list contains an infix operator used as nonfix
    | EqualPrecedence !a !a -- ^ The item list contains two operators with equal precedence but different associativities
    | Empty                 -- ^ The item list is empty
    deriving (Show, Eq, Ord, Read)

-- | Options provided for precedence parsing
data PrecParse m a = PrecParse
    { _precParseApply      :: a -> a -> m a                -- ^ How to apply a function to arguments
    , _precParseApplyInfix :: a -> a -> a -> m a           -- ^ How to apply an infix operator to arguments
    , _precParseFixval     :: a -> m Fixval                -- ^ The precedence + infix status of a token
    , _precParseError      :: forall b. PrecError a -> m b -- ^ Process an error
    -- , _precParseWarn       :: PrecError a -> m ()          -- ^ Process a warning
    }

data PrecStack a
    = INf !Int !a !(PrecStack a)
    | NONf !a !(PrecStack a)
    | NILf

-- | Run a precedence parser (O(n))
parse :: forall m a. Monad m => PrecParse m a -> [a] -> m a
parse (PrecParse apply applyInfix fixval error') items =
    let
        start :: a -> Fixval -> m (PrecStack a)
        start e Nonfix = pure $ NONf e NILf
        start e _ = error' $ StartsWithInfix e

        go :: PrecStack a -> a -> Fixval -> m (PrecStack a)
        go (NONf e r) e' Nonfix = NONf <$> apply e e' <*> pure r
        go p@INf{} e Nonfix = pure $ NONf e p
        go INf{} e _ = error' $ InfixUsedNonfix e
        go p@(NONf e1 (INf bp e2 (NONf e3 r))) e4 f@(Infix lbp rbp)
          | lbp > bp = pure $ INf rbp e4 p
          | lbp == bp = error' $ EqualPrecedence e2 e4
          | otherwise = do
              x <- applyInfix e2 e3 e1
              go (NONf x r) e4 f
        go p@NONf{} e' (Infix _ rbp) = pure $ INf rbp e' p
        go _ _ _ = error "PrecParse.parse"

        finish :: PrecStack a -> m a
        finish (NONf e1 (INf _ e2 (NONf e3 r))) = do
            x <- applyInfix e2 e3 e1
            finish (NONf x r)
        finish (NONf e1 NILf) = pure e1
        finish (INf _ e1 (NONf e2 p)) = error' $ EndsWithInfix e1
        finish _ = error "PrecParse.parse"

        loop :: [a] -> PrecStack a -> m (PrecStack a)
        loop [] s = pure s
        loop (x:xs) s = do
            f <- fixval x
            go s x f >>= loop xs
     in case items of
          [] -> error' Empty
          (x:xs) -> do
              f <- fixval x
              start x f >>= loop xs >>= finish 


