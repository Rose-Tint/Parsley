module Common.Rule (
    RuleName,
    RuleBody,
    Rule(..),
    evalRule,
) where

import Data.List.NonEmpty

import Common.Point


type RuleName = String

type RuleBody = NonEmpty Point

type RuleValue = ()

data Rule = Rule {
    rule_name :: RuleName,
    rule_body :: RuleBody,
    rule_value :: RuleValue
    }


evalRule :: Rule -> [Point] -> RuleValue
evalRule rule _ = rule_value rule
