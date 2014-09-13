{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleInstances, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes   #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeOperators      #-}
module Messaging.Macros (defineClass, definePhantomClass, idMarshaller) where
import Control.Applicative ((<$>))
import Control.Monad       (replicateM)
import Data.Typeable
import GHC.TypeLits        (Symbol)
import Messaging.Core
import Language.C.Inline.ObjC
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

defineClass :: String -> Maybe Name -> DecsQ
defineClass = definePhantomClass 0

definePhantomClass :: Int -> String -> Maybe Name -> DecsQ
definePhantomClass n name super = do
  let sym = litT $ strTyLit name
  vs <- replicateM n (newName "x")
  tyDec <- tySynD (mkName name) (map PlainTV vs) [t| Object $(sym) |]
  mClsDec <- genSubtypes sym (LitT . StrTyLit . nameBase <$> super)
  let tyCls = foldl1 appT (conT (mkName name) : map varT vs)
  let mName = mkName $ "marshal" ++ name
  marshalD <- sequence [sigD mName $ forallT (map PlainTV vs) (return []) [t| $tyCls -> IO $tyCls |]
                       ,valD (varP mName) (normalB [| return |]) []]
  return $ tyDec : mClsDec ++ marshalD

idMarshaller :: Name -> DecsQ
idMarshaller name = do
  VarI mName _ _ _ <- reify (mkName $ "marshal" ++ nameBase name)
  objc_marshaller mName mName

genSubtypes :: TypeQ -> Maybe Type -> DecsQ
genSubtypes _    Nothing      = return []
genSubtypes name (Just super) = do
  let geq = ''(:>)
      geqT = AppT (AppT (ConT geq) (ConT ''Symbol)) (ConT ''Symbol)
  ClassI _ insts <- reify ''(:>)
  let supers = [ typ | InstanceD [] (AppT (AppT geqT typ) sub) _ <- insts
                     , sub == super]
  concat <$> mapM (\s -> [d| instance $(return s) :> $name |]) (super:supers)

