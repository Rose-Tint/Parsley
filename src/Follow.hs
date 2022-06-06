{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Follow (
    FollowTable,
    getFollowers,
) where

import Control.Monad.Trans.Accum
import Data.List.NonEmpty (NonEmpty(..))
import Data.Foldable (fold)
import qualified Data.Map as M
import qualified Data.Set as S

import First
import Prod
import qualified ProdTable as PT


type FollowSet = S.Set Terminal

type FollowTable = M.Map NonTerminal (S.Set Terminal)

type Follow = Accum FollowTable


getFollowers :: PT.ProdTable -> FollowTable -> FollowTable
getFollowers tbl = execAccum $ mapM_ (\(n, rs) ->
    mapM_ (\r ->
        followRule (ruleBody r) n tbl
        ) rs
    ) (PT.assocs tbl)

followProdId :: ProdId -> PT.ProdTable -> Follow FollowSet
followProdId name tbl = looks (M.lookup (ProdNT name)) >>= \case
    Nothing -> case M.lookup name tbl of
        Nothing -> return mempty
        Just rules -> do
            fols <- mapM (\(_ :-> body) ->
                followRule body name tbl
                ) rules
            return $! fold fols
    Just fols -> return fols

followRule :: RuleBody -> ProdId -> PT.ProdTable -> Follow FollowSet
followRule (NonTerm name :| []) prod tbl = do
    fol <- followProdId prod tbl
    add (M.singleton name fol)
    return $! fol
followRule (NonTerm name :| [x]) _prod tbl = do
    let frsts = first x tbl
    add (M.singleton name frsts)
    return frsts
followRule (Terminal{} :| []) _prod _tbl = return mempty
followRule (_ :| (x:xs)) prod tbl =
    followRule (x :| xs) prod tbl
