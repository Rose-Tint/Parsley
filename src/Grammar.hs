module Grammar (
    Directive(..),
    Grammar(..),

    mkGrammar,
) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.IntMap as M

import Common.Code
import Common.Rule


data Directive = Directive

data Grammar = Grammar {
    gram_header :: Code,
    gram_footer :: Code,
    gram_dirs :: [Directive],
    gram_prodMap :: M.IntMap Prods,
    gram_lookupRule :: RuleName -> Maybe Rule,
    gram_lookupProd :: ProdId -> Maybe Prod,
    gram_head :: Rule
    }

mkGrammar :: Code -> [Directive] -> [Rule] -> Code
    -> Grammar
mkGrammar hdr dirs rules ftr = Grammar {
    gram_header = hdr,
    gram_footer = ftr,
    gram_dirs = dirs,
    gram_prodMap = prodMap,
    gram_lookupRule = lookupRule,
    gram_lookupProd = lookupProd,
    gram_head = head rules
    }
    where
        lookupProd = flip M.lookup prodMap
        lookupRule name =
            let eq = (== name) . prod_name
                (prods, _) = M.partition eq prodMap
            in case M.elems prods of
                [] -> Nothing
                (p:ps) -> Just (Rule name (p:|ps))
        -- insertRule (Rule name body) =
        --     M.insertWith addProds name body
        -- prodMap = foldr insertRule M.empty rules
        prodMap = M.empty
