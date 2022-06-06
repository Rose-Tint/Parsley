module ActionTable (
    ActionTable,
    Action(..),

    empty,
    singleton,
    insert,
    lookup,
    find,
    member,
    -- M.size,
    -- M.union,
    -- M.unionWith,
    -- M.unions,
    -- M.unionsWith,
    -- M.mapWithKey,
    -- M.foldrWithKey,
    -- M.foldMapWithKey,
    -- M.elems,
    -- M.keys,
    -- M.assocs,
    -- M.toAscList,
    -- M.toDescList,
    -- M.filter,
) where

import Prelude hiding (lookup)

import qualified Data.IntMap as IM
import qualified Data.Map as M

import Common.Action
import Common.Point
import Common.StateNum


type TermActMap = M.Map Terminal Action

type ActionTable = IM.IntMap TermActMap


empty :: ActionTable
empty = IM.empty

singleton :: StateNum -> TermActMap -> ActionTable
singleton = IM.singleton

insert :: StateNum -> Terminal -> Action -> ActionTable -> ActionTable
insert st term act = IM.alter (\case
    Nothing -> Just (M.singleton term act)
    Just tt -> Just (M.insert term act tt)
    ) st

lookup :: StateNum -> Terminal -> ActionTable -> Maybe Action
lookup st term tbl = IM.lookup st tbl >>= M.lookup term

find :: StateNum -> Terminal -> ActionTable -> Action
find s t = (M.! t) . (IM.! s)
    -- (tbl IM.! st) IM.! term

member :: StateNum -> Terminal -> ActionTable -> Bool
member st term tbl = case IM.lookup st tbl of
    Nothing -> False
    Just tt -> term `M.member` tt

