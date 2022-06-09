module ParserMonad (
    Parser,
    runParser,

    runLoop,
    shift,
    reduce,
    accept,
) where

import Common.Action
import Common.Code
import Common.Rule
import Common.StateNum
import ParserMonad.Internal


runLoop :: Parser Code
runLoop = getAction >>= \case
    Shift st' -> shift st' >> runLoop
    Reduce prod -> reduce prod >> runLoop
    Fail -> error "runLoop: Parser failed"
    Accept -> accept

-- | Push the current terminal onto the stack
-- and go to the given state.
shift :: StateNum -> Parser ()
shift st = do
    pushState st
    nextToken
    return ()

-- | Pop some stuff from the stack, call an
-- `evalProd` action over it, push the result
-- onto the stack, and return to the state
-- `GOTO(current, prod)`
reduce :: ProdId -> Parser ()
reduce prodId = do
    prod <- findProd prodId
    let name = prod_name prod
    tell [name]
    let prodLen = length (prod_body prod)
    popnStates prodLen
    prev <- peekStates
    _ts <- popnTerms prodLen
    -- let !_ = evalProd prod ts
    pushTerm (NonTerm name)
    goto prev name
    return ()

accept :: Parser RuleName
accept = getAllTerms >>= \case
    [] -> error "runLoop: empty parse-stack"
    [Term _] -> error $! "runLoop: " ++
        "final term is a terminal"
    [NonTerm name] -> return name
    _ -> error $! "runLoop: " ++
        "parse-stack not fully reduced"


