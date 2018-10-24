{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module DataTreePrint
  ( simplePrintTree
  , simplePrintTreeWithCustom
  , printTree
  , printTreeWithCustom
  , showTree
  , showTreeWithCustom
  , DataToLayouter(..)
  , LayouterF
  , NodeLayouter(..)
  , defaultLayouterF
  )
where



import           Data.Data
import           Text.PrettyPrint as PP

import           Data.Generics.Aliases
import           Data.Function (fix)

import           Data.Functor ((<$>))



-- | The "simple" printer does not try to fit more than one node into the
--   same line, even if it would fit.
simplePrintTree :: Data a => a -> Doc
simplePrintTree = runDataToDoc (fix defaultToDocF)

-- | Allows to specialize the transformation for specific types. Use `syb`'s
-- `extQ` function(s). See the source of `defaultLayouterF` for an
-- example of how to do this.
simplePrintTreeWithCustom :: Data a => ToDocF -> a -> Doc
simplePrintTreeWithCustom toDocF = runDataToDoc (fix toDocF)

-------
-------

-- | Somewhat more intelligent printer that tries to fit multiple nodes
--   into the same line there is space given the specified number of total
--   columns.
--   For example, `(1,2,3)` will be printed as "(,,) (1) (2) (3)" instead
--   of "(,,)\n  1\n  2\n  3". Parentheses are added in these cases to prevent
--   syntactic ambiguities.
printTree :: forall a . Data a => Int -> a -> Doc
printTree startIndent node =
  _lay_func (runDataToLayouter (fix defaultLayouterF) node) (Right startIndent)

printTreeWithCustom :: Data a => Int -> LayouterF -> a -> Doc
printTreeWithCustom startIndent layoutF node =
  _lay_func (runDataToLayouter (fix layoutF) node) (Right startIndent)

showTree :: Data a => a -> String
showTree = render . printTree 100

showTreeWithCustom :: Data a => LayouterF -> a -> String
showTreeWithCustom layoutF node = render $ printTreeWithCustom 100 layoutF node

-- | This newtype is necessary so `fix` can be used in combination with
--   the constrained forall-quantification.
newtype DataToDoc = DataToDoc
  { runDataToDoc :: forall a . Data a => a -> Doc }

type ToDocF = DataToDoc -> DataToDoc

data NodeLayouter = NodeLayouter
  { _lay_llength :: Int -- ^ the length of this node, if printed
                        --   on a single line
  , _lay_needsParens :: Bool
  , _lay_func :: Either Bool Int -> Doc
                 -- ^ Left: one-line output, the boolean
                 -- indicates if parentheses are advisable
                 -- given the context. (They can be omitted
                 -- in cases like when there is only one
                 -- constructor).
                 -- Right: The Int is the remaining vertical
                 -- space left for this node.
  }

-- | This newtype is necessary so `fix` can be used in combination with
--   the constrained forall-quantification.
newtype DataToLayouter = DataToLayouter
  { runDataToLayouter :: forall a . Data a => a -> NodeLayouter }

type LayouterF = DataToLayouter -> DataToLayouter

defaultToDocF :: ToDocF
defaultToDocF (DataToDoc lf) = DataToDoc $ genLayouter `ext1Q` listLayouter
                                                       `extQ` string
  where
    genLayouter n =
      let cStr = showConstr $ toConstr n
          childrenDoc = gmapQ lf n
      in  text cStr $$ nest 2 (vcat childrenDoc)
    listLayouter :: forall b . Data b => [b] -> Doc
    listLayouter [] = text "[]"
    listLayouter (x1:xr) = text "[" $$ nest 2 d1
                        $$ vcat [text "," $$ nest 2 d | d <- dr]
                        $$ text "]"
      where
        d1 = lf x1
        dr = lf <$> xr
    string :: String -> Doc
    string s = text $ show s

defaultLayouterF :: LayouterF
defaultLayouterF (DataToLayouter lf) = DataToLayouter
                                     $ genLayouter `ext1Q` listLayouter
                                                   `extQ` string
  where
    genLayouter :: forall b . Data b => b -> NodeLayouter
    genLayouter n = NodeLayouter llen needParens func
      where
        cs = show $ toConstr n
        subs = gmapQ lf n
        llen = length cs
             + length subs
             + sum [ if _lay_needsParens s
                       then _lay_llength s + 2
                       else _lay_llength s
                   | s <- subs
                   ]
        needParens = not $ null subs
        func (Right i)
          | llen<=i = text cs <+> hsep [_lay_func s (Left True) | s <- subs]
          | otherwise =  text cs
                      $$ nest 2 (vcat [_lay_func s (Right $ i-2) | s <- subs])
        func (Left True)
          = (if null subs then id else parens)
          $ text cs <+> hsep [_lay_func s (Left True) | s <- subs]
        func (Left False)
          = text cs <+> hsep [_lay_func s (Left True) | s <- subs]
    listLayouter :: forall b . Data b => [b] -> NodeLayouter
    listLayouter [] = NodeLayouter 2 False $ \_ -> text "[]"
    listLayouter xs@(_:_) = NodeLayouter llen False func
      where
        subs@(s1:sr) = lf <$> xs
        llen = 1
             + length subs
             + sum (_lay_llength <$> subs)
        func (Right i)
          | llen<=i = text "["
                   PP.<> hcat (punctuate (text ",") [_lay_func s (Left False) | s <- subs])
                   PP.<> text "]"
          | otherwise = text "[" $$ nest 2 (_lay_func s1 (Right $ i-2))
                     $$ vcat [text "," $$ nest 2 (_lay_func s (Right $ i-2)) | s <- sr]
                     $$ text "]"
        func (Left _)
          = func (Right 99999999)
    string :: String -> NodeLayouter
    string s = NodeLayouter (length s') False $ \_ -> text $ s'
      where s' = show s
