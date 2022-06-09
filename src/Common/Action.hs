module Common.Action (
    Action(..),
) where

import Common.Rule
import Common.StateNum


data Action
    = Shift StateNum
    | Reduce ProdId
    | Fail
    | Accept
