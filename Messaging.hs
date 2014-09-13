{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleInstances            #-}
{-# LANGUAGE FunctionalDependencies, MagicHash, MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds, QuasiQuotes, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                 #-}
module Messaging where
import Data.Typeable          (Typeable)
import Data.Typeable.Internal
import GHC.TypeLits           (Symbol)
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

objc_import ["<Cocoa/Cocoa.h>"]

data ObjC = NSObject
          | NSResponder
          | NSView
          | NSControl
          | NSTextField
            deriving (Typeable, Read, Show, Eq, Ord)

newtype Object (a :: k) = Object (ForeignPtr ())
                        deriving (Typeable, Show, Eq)

-- | @a ':>' b@ indicates that @a@ is a super-class of @b@.
class (a :: k) :> (b :: k2)

type NSObject    = Object 'NSObject
type NSView      = Object 'NSView
type NSResponder = Object 'NSResponder
type NSControl   = Object 'NSControl
type NSTextField = Object 'NSTextField

deriving instance Typeable 'NSObject
deriving instance Typeable 'NSControl
deriving instance Typeable 'NSTextField

instance (a :> b, c :> d) => (b -> c) :> (a -> d)
instance a :> a
instance (a :> b) => [a] :> [b]

class Selector cls msg | msg -> cls where
  data Message (cls :: k) (msg :: k') :: *
  type Returns cls msg :: *
  send' :: Object cls -> Message cls msg -> Returns cls msg

cast :: (a :> b) => Object b -> Object a
cast (Object ptr) = Object ptr

instance 'NSObject    :> 'NSResponder
instance 'NSObject    :> 'NSControl
instance 'NSObject    :> 'NSTextField
instance 'NSResponder :> 'NSControl
instance 'NSResponder :> 'NSTextField
instance 'NSControl   :> 'NSTextField

objc_typecheck

instance Selector 'NSControl "intValue" where
  data Message 'NSControl "intValue"  = IntValue
  type Returns 'NSControl "intValue" = IO Int
  send' ctrl IntValue = $(objc ['ctrl :> Class ''NSControl] $ ''Int <: [cexp| [ctrl intValue] |])

instance Selector 'NSControl "setIntValue" where
  data Message 'NSControl "setIntValue"  = SetIntValue Int
  type Returns 'NSControl "setIntValue" = IO ()
  send' ctrl (SetIntValue i)
    = $(objc ['ctrl :> Class ''NSControl, 'i :> ''Int] $ void [cexp| [ctrl setIntValue: i] |])

send :: (a :> b, Selector a msg) => Object b -> Message a msg -> Returns a msg
send = send' . cast

infixl 1 #
(#) :: (a :> b, Selector a msg) => Object b -> Message a msg -> Returns a msg
(#) = send

objc_emit
