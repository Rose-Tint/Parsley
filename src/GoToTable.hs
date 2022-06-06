module GoToTable (
    GoToTable,
    lookup,
    find,
) where

import Prelude hiding (lookup)

import qualified Data.IntMap as IM
import qualified Data.Map as M

import Common.Point
import Common.StateNum


type GoToTable = IM.IntMap (M.Map NonTerm StateNum)


lookup :: StateNum -> NonTerm -> GoToTable -> Maybe StateNum
lookup st nt tbl = IM.lookup st tbl >>= M.lookup nt

find :: StateNum -> NonTerm -> GoToTable -> StateNum
find st nt = (M.! nt) . (IM.! st)
