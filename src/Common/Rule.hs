module Common.Rule (
    Token,
    Terminal(..),
    Term(..),
    RuleName,
    Prods,
    Rule(..),
    ProdId,
    ProdBody,
    Prod(..),

    ruleRange,
    seqRuleBody,
    seqProds,
    fromPrevProd,
    addProds,
) where

import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NE

import Common.Code


type Token = String

data Term
    = Term Terminal
    | NonTerm RuleName
    deriving (Eq, Ord)

data Terminal
    = TokenTerm Token
    | StartTerm
    | EndTerm
    deriving (Eq, Ord)

type RuleName = String

type Prods = NonEmpty Prod

data Rule = Rule {
    rule_name :: RuleName,
    rule_prods :: Prods
    }

type ProdId = Int

type ProdBody = [Term]

data Prod = Prod {
    prod_name :: RuleName,
    prod_id :: ProdId,
    prod_body :: ProdBody,
    prod_code :: Code
    }


ruleRange :: Rule -> (ProdId, ProdId)
ruleRange (Rule _name prods) =
    let low = prod_id (NE.head prods)
        high = prod_id (NE.last prods)
    in (low, high)

seqRuleBody :: Int -> Rule -> Rule
seqRuleBody n rule@(Rule _ prods) = rule {
    rule_prods = seqProds n prods
    }

seqProds :: Int -> Prods -> Prods
seqProds n! (p:|[]) = p { prod_id = n } :|[]
seqProds n! (p1:|p2:ps) =
    p1' <| seqProds (n + 1) (p2:|ps)
    where
        p1' = p1 { prod_id = n }

fromPrevProd :: Prod -> Prod -> Prod
fromPrevProd prev new = new {
    prod_id = prod_id prev + 1
    }

addProds :: Prods -> Prods -> Prods
addProds news (old:|[]) =
    let news' = seqProds (prod_id old + 1) news
    in old <| news'
addProds news (old1:|old2:olds) =
    let olds' = old2 :| olds
    in old1 <| addProds news olds'
