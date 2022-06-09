module ParserMonad.Internal (
    Parser,
    runParser,
    tell,

    nextToken,

    getAction,
    findProd,
    goto,

    peekStates,
    popnStates,
    pushState,

    peekTerms,
    popnTerms,
    pushTerm,
    getAllTerms,
) where

import Control.Monad (when)
import Control.Monad.Trans.RWS
import Data.List (uncons)

import Common.Action
import Common.Rule
import Common.StateNum
import Grammar
import Table.Trans


type Stream = [Token]

type StateStack = [StateNum]

type TermStack = [Term]

data ParseState = PS {
    ps_input :: Stream,
    ps_states :: StateStack,
    ps_terms :: TermStack,
    ps_lookAhead :: Terminal
    }

data Tables = Tables {
    actions :: Actions,
    gotos :: Gotos,
    grammar :: Grammar
    }

-- the `Writer` output is the 'history' of production
-- applications
type Parser = RWS
    Tables
    [RuleName]
    ParseState


runParser :: Grammar -> Stream -> Parser a -> a
runParser gr ts p = fst (evalRWS p tbls st)
    where
        tbls = uncurry Tables (mkTables gr) gr
        st = PS ts [0] [] StartTerm

nextToken :: Parser Token
nextToken = do
    (next, rest) <- gets (uncons . ps_input) >>= \case
        Nothing -> error "shift: unexpected EOF"
        Just res -> return res
    modify $ \s -> s {
        ps_input = rest,
        ps_lookAhead = TokenTerm next
        }
    return next;

getAction :: Parser Action
getAction = do
    st <- peekStates
    la <- gets ps_lookAhead
    act <- asks (lookupAction st la . actions)
    return act

findProd :: ProdId -> Parser Prod
findProd id' = do
    mProd <- asks (flip gram_lookupProd id' . grammar)
    case mProd of
        Nothing -> error $! "findProd: " ++
            "could not find any productions for \"" ++
            show id' ++ "\""
        Just prod -> return prod

goto :: StateNum -> RuleName -> Parser ()
goto st term = do
    mGoto <- asks (lookupGoto st term . gotos)
    case mGoto of
        Nothing -> error "goto: no state found"
        Just st' -> pushState st'

peekTerms :: Parser Term
peekTerms = gets ps_terms >>= \case
    [] -> error "peekTerms: empty value-stack"
    (pt:_) -> return pt

peekStates :: Parser StateNum
peekStates = gets ps_states >>= \case
    [] -> error "peekStates: empty state-stack"
    (st:_) -> return st

popnTerms :: Int -> Parser [Term]
popnTerms n = do
    (pre, post) <- gets (splitAt n . ps_terms)
    when (length pre > n) $
        error "popnTerms: pop-count > stack-size"
    modify $ \s -> s { ps_terms = post }
    return pre

popnStates :: Int -> Parser [StateNum]
popnStates n = do
    (pre, post) <- gets (splitAt n . ps_states)
    when (length pre > n) $
        error "popnStates: pop-count > stack-size"
    modify $ \s -> s { ps_states = post }
    return pre

pushTerm :: Term -> Parser ()
pushTerm p = modify $ \s -> s {
    ps_terms = (p:ps_terms s)
    }

pushState :: StateNum -> Parser ()
pushState st = modify $ \s -> s {
    ps_states = (st:ps_states s)
    }

getAllTerms :: Parser TermStack
getAllTerms = gets ps_terms
