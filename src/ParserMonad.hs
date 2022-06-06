module ParserMonad (
    module ParserMonad
) where

import Control.Monad (when)
import Control.Monad.Trans.RWS
import Data.List (uncons)

import qualified ActionTable as AT
import Common.Action
import Common.Point
import Common.Rule
import Common.StateNum
import qualified GoToTable as GT


type StateStack = [StateNum]

type ParseStack = [Point]

data ParseState = PS {
    ps_input :: [Token],
    ps_stateStk :: StateStack,
    ps_parseStk :: ParseStack,
    ps_lookAhead :: Terminal
    }

data Tables = Tables {
    tbls_actions :: AT.ActionTable,
    tbls_gotos :: GT.GoToTable
    }

-- the `Writer` output is the 'history' of rule
-- applications
type Parser = RWS Tables [RuleName] ParseState


runLoop :: Parser Point
runLoop = getAction >>= \case
    Shift st' -> shift st' >> runLoop
    Reduce rule -> reduce rule >> runLoop
    Accept -> gets ps_parseStk >>= \case
        [] -> error "runLoop: empty parse-stack"
        [x] -> return x
        (_:_) -> error $ "runLoop: " ++
            "parse-stack not fully reduced"

getAction :: Parser Action
getAction = do
    st <- peekStates
    la <- gets ps_lookAhead
    asks (AT.find st la . tbls_actions)

-- | Push the current terminal onto the stack
-- and go to the given state.
shift :: StateNum -> Parser ()
shift st = do
    pushState st
    (next, rest) <- gets (uncons . ps_input) >>= \case
        Nothing -> error "shift: unexpected EOF"
        Just res -> return res
    modify $ \s -> s {
        ps_input = rest,
        ps_lookAhead = TokenTerm next
        }
    return ()

-- | Pop some stuff from the stack, call an
-- `applyRule` action over it, push the result
-- onto the stack, and return to the state
-- `GOTO(current, prod)`
reduce :: Rule -> Parser ()
reduce rule = do
    let name = rule_name rule
        ruleLen = length (rule_body rule)
    tell [name]
    popnStates ruleLen
    prior <- peekStates
    pts <- popnPoints ruleLen
    let !_ = evalRule rule pts
    pushPoint (NonTerm name)
    st' <- asks (GT.find prior name . tbls_gotos)
    pushState st'
    return ()

peekPoints :: Parser Point
peekPoints = gets ps_parseStk >>= \case
    [] -> error "peekPoints: empty value-stack"
    (pt:_) -> return pt

peekStates :: Parser StateNum
peekStates = gets ps_stateStk >>= \case
    [] -> error "peekStates: empty state-stack"
    (st:_) -> return st

popnPoints :: Int -> Parser [Point]
popnPoints n = do
    (pre, post) <- gets (splitAt n . ps_parseStk)
    when (length pre > n) $
        error "popnPoints: pop-count > stack-size"
    modify $ \s -> s { ps_parseStk = post }
    return pre

popnStates :: Int -> Parser [StateNum]
popnStates n = do
    (pre, post) <- gets (splitAt n . ps_stateStk)
    when (length pre > n) $
        error "popnPoints: pop-count > stack-size"
    modify $ \s -> s { ps_stateStk = post }
    return pre

pushPoint :: Point -> Parser ()
pushPoint p = modify $ \s -> s {
    ps_parseStk = (p:ps_parseStk s)
    }

pushState :: StateNum -> Parser ()
pushState st = modify $ \s -> s {
    ps_stateStk = (st:ps_stateStk s)
    }

-- | Will only be used when `... X . $`. Signals
-- an acceptance of the input stream.
-- accept :: Parser ()

-- | Signals that a conflicting set of outcomes
-- are possible for this grammar.
-- conflict :: Action -> Action -> Parser Action
