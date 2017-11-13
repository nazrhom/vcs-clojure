{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Util.PPPatch where

import VCS.Multirec
import Clojure.Lang
import Clojure.PrettyPrint
import Data.Type.Equality hiding (apply)

showPatchEffect :: (IsRecEl u, IsRecEl v) => Almu u v -> Usingl u -> String
showPatchEffect (Alspn s) e =
  showSpineEffect
    (showAtEffect showAlmuHEffect)
    (showAlEffect (showAtEffect showAlmuHEffect))
    e
    s
showPatchEffect (Alins c ctx) e = "{+" ++ show (ppConstr c) ++ showCtxPosE ctx e
showPatchEffect (Aldel c ctx) e = case view e of
  (Tag c' d) -> case testEquality' c c' of
    Nothing -> "error"
    Just (Refl, Refl) -> "{-" ++ show (ppConstr c) ++ showCtxNegE ctx d


showCtxPosE :: IsRecEl u => Ctx (AtmuPos u) l -> Usingl u -> String
showCtxPosE (Here r p) u = "+}\n" ++ showAtmuPosE u r ++ "\n{+" ++ show p ++ "+}"
showCtxPosE (There u' ctx) u = show u' ++ " " ++ showCtxPosE ctx u

showCtxNegE :: IsRecEl v => Ctx (AtmuNeg v) l -> All Usingl l -> String
showCtxNegE (Here r p) (u `Ac` us) = "-}\n" ++ showAtmuNegE u r ++ "\n{-" ++ show p ++ "-}"
showCtxNegE (There u' ctx) (u `Ac` us) = show u' ++ " " ++ showCtxNegE ctx us

showSpineEffect :: IsRecEl u => (forall u . Usingl u -> at u -> String)
                -> (forall p1 p2 . All Usingl p1 -> al p1 p2 -> String)
                -> Usingl u -> Spine at al u -> String
showSpineEffect showAtE showAlE e Scp = show e
showSpineEffect showAtE showAlE e (Scns i p) = case view e of
  (Tag c d) -> case testEquality c i of
    Nothing -> undefined
    Just Refl -> show (ppConstr c) ++ showAll showAtE d p
      where
        showAll :: (forall u . Usingl u -> at u -> String)
                  -> All Usingl l -> All (at :: U -> *) l -> String
        showAll showAtE An An = ""
        showAll showAtE (e `Ac` es) (at `Ac` ats) = showAtE e at ++ " " ++ showAll showAtE es ats
showSpineEffect showAtE showAlE e (Schg i j p) = case view e of
  (Tag c d) -> case testEquality c i of
    Just Refl ->
      "(" ++ show (ppConstr i) ++ " -> " ++ show (ppConstr j) ++ ")" ++ showAlE d p
    Nothing -> undefined

showAlEffect :: (forall u . Usingl u -> at u -> String)
             -> All Usingl p1 -> Al at p1 p2  -> String
showAlEffect showAtE An A0 = ""
showAlEffect showAtE (u `Ac` us) (Amod u' al) = "[%" ++ showAtE u u' ++ "%]" ++ showAlEffect showAtE us al
showAlEffect showAtE (u `Ac` us) (Adel u' al) = "[-" ++ show u' ++ "-]" ++ showAlEffect showAtE us al
showAlEffect showAtE us (Ains u' al) = "[+" ++ show u' ++ "+]" ++ showAlEffect showAtE us al

showPair :: Usingl u -> Usingl v -> String
showPair u v = show u ++ ", " ++ show v

showAtEffect :: (IsRecEl u => Usingl u -> rec u -> String)
              -> Usingl u -> At rec u -> String
showAtEffect showREffect u (As pair) = show pair
showAtEffect showREffect u (Ai spmu) = showREffect u spmu

showAlmuHEffect :: IsRecEl u => Usingl u -> AlmuH u -> String
showAlmuHEffect u (AlmuH almu) = showPatchEffect almu u

showAtmuPosE :: (IsRecEl u, IsRecEl v) => Usingl u -> AtmuPos u v -> String
showAtmuPosE u (FixPos almu) = showPatchEffect almu u

showAtmuNegE :: (IsRecEl u, IsRecEl v) => Usingl v -> AtmuNeg u v -> String
showAtmuNegE u (FixNeg almu) = showPatchEffect almu u
