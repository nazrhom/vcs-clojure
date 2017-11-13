{-# LANGUAGE AllowAmbiguousTypes #-}

module Patches.Diff3 where

import Clojure.AST
import Clojure.Lang

import VCS.Multirec


change a b = As (Contract (a, b))
keep a = change a a
recurse a = Ai (AlmuH a)

variable a = (Term (TaggedString "Var" a emptyRange) emptyRange)
nil = (Nil emptyRange)
cons a b c = Cons a b c emptyRange

trash1Line = UExpr (Collection "Parens"
            (cons (variable "trash1")
            "Space"
            (cons (variable "trash")
            "Space"
            (cons (variable "trash")
            "Space"
            nil ))) emptyRange)

trash2Line = UExpr (Collection "Parens"
            (cons (variable "trash3")
            "Space"
            (cons (variable "trash")
            "Space"
            nil)) emptyRange)

lvl1 :: (Almu (ToSing Expr) (ToSing Expr))
lvl1 = Alspn
      (Scns C3CollectionProof
        (keep (UString "Parens") .@.
         recurse lvl2 .@.
        An))

lvl2 = Alspn
      (Scns C1ConsProof
        (recurse (Alspn Scp) .@.
         keep (UString "Space") .@.
         recurse lvl3 .@.
        An))

lvl3 = Alspn
      (Scns C1ConsProof
        (recurse (Alspn Scp) .@.
         keep (UString "NewLine") .@.
         recurse lvl4 .@.
        An))

lvl4 = Alspn
      (Scns C1ConsProof
        (recurse (Alspn Scp) .@.
         keep (UString "Space") .@.
         recurse lvl5 .@.
        An))

lvl5 = Aldel
      C1ConsProof
      (There trash1Line
      (There (UString "NewLine")
      (Here (FixNeg (lvl6)) An)))

lvl6 = Alspn
      (Scns C1ConsProof
        (recurse lvl7 .@.
         keep (UString "NewLine") .@.
         recurse lvl8_Del .@.
         An))

lvl7 = Alspn
      (Scns C3CollectionProof
        (keep (UString "Parens") .@.
         recurse lvl9 .@.
         An) )
-----------
lvl9 = Alspn
      (Scns C1ConsProof
        (recurse (Alspn Scp) .@.
         keep (UString "Space") .@.
         recurse lvl10 .@.
         An))

lvl10 = Alspn
      (Scns C1ConsProof
        (recurse (Alspn Scp) .@.
         keep (UString "Space") .@.
         recurse tmp .@.
         An))

tmp = Alspn
     (Scns C1ConsProof
       (recurse (Alspn Scp) .@.
        keep (UString "Space") .@.
        recurse lvl11 .@.
        An))

lvl11 = Alspn
      (Schg C1NilProof C1ConsProof
        (Ains (UExpr (Term (TaggedString "Var" "new" emptyRange) emptyRange)) (
         Ains (UString "Space") (
         Ains (USepExprList (Nil emptyRange)) (
         A0)))))
------------
lvl8 = Alspn
      (Scns C1ConsProof
        (recurse lvl12 .@.
        keep (UString "NewLine") .@.
        recurse lvl17 .@.
        An))

lvl8_Del = Aldel
        C1ConsProof
        (There trash2Line
        (There (UString "NewLine")
        (Here (FixNeg (Alspn (Scp))) An)))

lvl12 = Alspn
        (Scns C3CollectionProof
          (keep (UString "Parens") .@.
           recurse lvl13 .@.
           An))

lvl13 = Alspn
      (Scns C1ConsProof
        (recurse lvl14 .@.
        keep (UString "Space") .@.
        recurse lvl15 .@.
        An))

lvl14 = Alspn
      (Scns C3TermProof
        (recurse (Alspn (Scns C6TaggedStringProof
                                (keep (UString "Var") .@.
                                change (UString "trash3") (UString "keep4") .@.
                                An))
                    ) .@.
        An))

lvl15 = Alspn
      (Scns C1ConsProof
        (recurse lvl16 .@.
         keep (UString "Space") .@.
         recurse (Alspn Scp) .@.
         An))

lvl16 = Alspn
      (Scns C3TermProof
        (recurse (Alspn (Scns C6TaggedStringProof
                                (keep (UString "Var") .@.
                                change (UString "trash") (UString "keep") .@.
                                An))
                    ) .@.
        An))

lvl17 = Alspn
      (Schg C1ConsProof C1NilProof
        (Adel keep4 (
         Adel (UString "NewLine") (
         Adel (USepExprList (Nil emptyRange)) A0
         ))))

keep4 = UExpr (Collection "Parens" (Cons
          (Term (TaggedString "Var" "keep4" emptyRange) emptyRange)
          "Space"
          (Cons (Term (TaggedString "Var" "keep" emptyRange) emptyRange)
          "Space"
          (Nil emptyRange) emptyRange) emptyRange) emptyRange)