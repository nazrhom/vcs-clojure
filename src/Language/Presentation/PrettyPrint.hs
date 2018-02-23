{-# LANGUAGE GADTs #-}

module Language.Presentation.PrettyPrint where

import Text.PrettyPrint.Leijen
import Language.Presentation.Lang
import Language.Presentation.AST

ppConstr :: ConstrFor u c -> Doc
ppConstr ParensProof = text "Parens"
ppConstr ValProof = text "Val"
ppConstr SumProof = text "Sum"
ppConstr AtomProof = text "Atom"

ppUsingl :: Usingl u -> Doc
ppUsingl (UInt i) = text (show i)
ppUsingl (UT t) = ppT t
ppUsingl (UA a) = ppA a

ppT :: T -> Doc
ppT (Sum t1 t2) = ppT t1 <> ppT t2
ppT (Atom a)     = ppA a

ppA :: A -> Doc
ppA (Parens t) = parens (ppT t)
ppA (Val v) = text (show v)