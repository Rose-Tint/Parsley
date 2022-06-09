module Table.Trans.Item (
    Item(..),
    ItemSet,

    mkStartItem,
    eofTerm,

    ruleToItems,
    prodToItem,

    incDot,
    locus,
    getLoci,
    locusIs,

    isReducable,
    preEOF,
) where

import qualified Data.List.NonEmpty as NE (toList)
import qualified Data.Set as S

import Common.Rule


-- | An item's position
type Dot = Int

data Item = Item {
    item_id :: Int,
    item_dot :: Dot,
    item_terms :: [Term]
    }
    deriving (Eq, Ord)

type ItemSet = S.Set Item


mkStartItem :: RuleName -> Item
mkStartItem name = Item 0 0 [NonTerm name, eofTerm]

eofTerm :: Term
eofTerm = Term EndTerm

ruleToItems :: Rule -> ItemSet
ruleToItems rule = S.fromList list
    where
        list = NE.toList (fmap prodToItem prods)
        prods = rule_prods rule

prodToItem :: Prod -> Item
prodToItem prod = Item (prod_id prod) 0 (prod_body prod)

incDot :: Item -> Item
incDot it = it { item_dot = item_dot it + 1 }

-- | An item's 'locus' is the term immediately
-- following its dot-position
locus :: Item -> Maybe Term
locus (Item _ dot terms)
    | length terms > dot = Just (terms !! dot)
    | otherwise = Nothing

getLoci :: ItemSet -> S.Set Term
getLoci = foldr (\item set -> case locus item of
    Nothing -> set
    Just term -> S.insert term set
    ) S.empty

locusIs :: Term -> Item -> Bool
locusIs term item = maybe False (== term) (locus item)

isReducable :: Item -> Bool
isReducable (Item _ dot terms) = dot >= length terms

preEOF :: Item -> Bool
preEOF = locusIs (Term EndTerm)
