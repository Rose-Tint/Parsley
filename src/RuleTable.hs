module RuleTable (
    RuleTable,

    M.empty,
    M.singleton,
    M.insert,
    M.lookup,
    find,
    M.member,
    M.size,
    M.union,
    M.unionWith,
    M.unions,
    M.unionsWith,
    M.mapWithKey,
    M.foldrWithKey,
    M.foldMapWithKey,
    M.elems,
    M.keys,
    M.assocs,
    M.toAscList,
    M.toDescList,
    M.filter,
) where

import qualified Data.Map as M

import Common.Rule


type RuleTable = M.Map RuleName Rule


find :: RuleName -> RuleTable -> Rule
find = flip (M.!)
