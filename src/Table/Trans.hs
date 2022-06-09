module Table.Trans (
    Grammar(..),
    Actions,
    Gotos,

    mkTables,
    lookupAction,
    lookupGoto,
) where

import Control.Monad (foldM)
import Control.Monad.Trans.RWS
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Common.Action
import Common.Rule
import Common.StateNum
import Grammar
import Table.Trans.Item


type Trans = RWS
    Grammar
    [TableQuad]
    StateNum

type TableQuad = (StateNum, Term, StateNum, ItemSet)

type TransMap = M.Map Term (StateNum, ItemSet)

type TransTable = A.Array StateNum TransMap


mkTables :: Grammar -> (Actions, Gotos)
mkTables gr = (acts, gotos)
    where
        acts = mkActions tbl
        gotos = mkGotos tbl
        (end, trips) = execRWS run gr 0
        tbl = mkTransTable trips end
        run = do
            start <- mkStartSet
            _ <- trans start
            return ()

closeItem :: Item -> Trans ItemSet
closeItem item = do
    lookup' <- asks gram_lookupRule
    case locus item of
        Just (NonTerm name) -> case lookup' name of
            Just rule -> closeSet (ruleToItems rule)
            Nothing -> error $! "closeItem: " ++
                "rule not found (" ++ name ++ ")"
        _ -> return $! S.singleton item

closeSet :: ItemSet -> Trans ItemSet
closeSet set0 = foldM (\set1 item -> do
    set2 <- closeItem item
    return $! set2 <> set1
    ) set0 set0

{- wikipedia.org/wiki/LR_parser#Table_construction
1) Take the subset, `items`, of all items in
   `orig` where there is a dot in front of the
   symbol of interest `sym`.
2) For each item in `items`, move the dot to the right
   of `sym`.
3) Close the resulting set of items.
-}
leadsFrom :: Term -> ItemSet -> Trans ItemSet
leadsFrom sym orig = do
    let items = S.filter (locusIs sym) orig
    let items' = S.map incDot items
    closeSet items'

-- | Returns the state number of its given item-set
trans :: ItemSet -> Trans StateNum
trans orig = do
    from <- get
    let terms = getLoci orig
    _ <- mapM_ (\term -> do
        set <- leadsFrom term orig
        modify (+1)
        to <- trans set
        tell [(from, term, to, set)]
        ) terms
    return from

mkStartSet :: Trans ItemSet
mkStartSet = do
    name <- asks (rule_name . gram_head)
    let item = mkStartItem name
    state0 <- closeSet (S.singleton item)
    return $! state0

initTable :: StateNum -> TransTable
initTable end =
    let list = [(i, M.empty) | i <- [0..end]]
    in A.array (0, end) list

mkTransTable :: [TableQuad] -> StateNum -> TransTable
mkTransTable trips end = foldr (
    \(from, term, to, set) tbl ->
        let row = tbl A.! from
            row' = M.insert term (to, set) row
        in tbl A.// [(from, row')]
    ) (initTable end) trips

type ActMap = M.Map Terminal Action

type Actions = A.Array StateNum ActMap

mkAction :: StateNum -> ItemSet -> Action
mkAction st set = case L.find isReducable list of
    Nothing -> Shift st
    Just item
        | preEOF item -> Accept
        | otherwise -> Reduce (item_id item)
    where
        list = S.elems set

mkActions :: TransTable -> Actions
mkActions = fmap mkActMap
    where
        mkActMap :: TransMap -> ActMap
        mkActMap mp = M.fromList [
            (t, mkAction st set)
                | (Term t, (st, set)) <- M.assocs mp
            ]

lookupAction :: StateNum -> Terminal -> Actions -> Action
lookupAction st term acts = maybe Fail id $!
    M.lookup term (acts A.! st)

type GotoMap = M.Map RuleName StateNum

type Gotos = A.Array StateNum GotoMap

mkGotos :: TransTable -> Gotos
mkGotos = fmap mkGotoMap
    where
        mkGotoMap :: TransMap -> GotoMap
        mkGotoMap mp = M.fromList [
            (x, st) | (NonTerm x, (st, _)) <- M.assocs mp
            ]

lookupGoto :: StateNum -> RuleName -> Gotos -> Maybe StateNum
lookupGoto st name gts = M.lookup name (gts A.! st)
