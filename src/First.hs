{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module First (
    FirstSet,
    first,
) where

import Data.List.NonEmpty
import qualified Data.Set as S

import Prod
import qualified ProdTable as PT


type FirstSet = S.Set Terminal

-- type FirstMap = M.Map ProdId FirstSet


class First a where
    first :: a -> PT.ProdTable -> FirstSet


instance First ProdId where
    first nm tbl =
        let rs = PT.find nm tbl
        in foldMap ((`first` tbl) . ruleBody) rs

instance First NonTerminal where
    first (ProdNT prod) = first prod

instance First Point where
    first (Terminal term) _ = S.singleton term
    first (NonTerm name) tbl = first name tbl

-- effectively implements `first` for a `Rule`
-- without having to deal with extensions
instance First a => First (NonEmpty a) where
    first (x :| _) = first x
