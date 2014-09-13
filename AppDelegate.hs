{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleInstances    #-}
{-# LANGUAGE MultiParamTypeClasses, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies                                        #-}
module AppDelegate (objc_initialise) where
import Messaging

import Control.Applicative
import Data.Typeable          (Typeable)
import FRP.Sodium
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

objc_import ["<Cocoa/Cocoa.h>"]

defineClass "NSObject"    Nothing
idMarshaller ''NSObject

defineClass "NSString"    (Just ''NSObject)
idMarshaller ''NSString

defineClass "NSControl"   (Just ''NSObject)
idMarshaller ''NSControl

defineClass "NSTextField" (Just ''NSControl)
idMarshaller ''NSTextField

instance Selector "NSControl" "intValue" where
  type Returns "intValue" = IO Int
  data Message "intValue" = IntValue
  send' ctrl IntValue = $(objc ['ctrl :> ''NSControl] $ ''Int <: [cexp| [ctrl intValue] |])

instance Selector "NSControl" "setIntValue" where
  type Returns "setIntValue" = IO ()
  data Message "setIntValue" = SetIntValue Int
  send' ctrl (SetIntValue i)
    = $(objc ['ctrl :> ''NSControl, 'i :> ''Int] $ void [cexp| [ctrl setIntValue: i] |])

type Listener a = a -> IO ()

data Session = Session { dollarsChanged_ :: Listener Int
                       , rateChanged_    :: Listener Int
                       } deriving (Typeable)

rateChanged :: Session -> Int -> IO ()
rateChanged = rateChanged_

dollarsChanged :: Session -> Int -> IO ()
dollarsChanged = dollarsChanged_

objc_typecheck

marshTF :: NSTextField -> IO NSTextField
marshTF = return

objc_marshaller 'marshTF 'marshTF

nsLog :: String -> IO ()
nsLog msg = $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

newSession :: NSTextField -> IO Session
newSession tf = sync $ do
    (dolBh, dolL) <- newBehaviour 0
    (ratBh, ratL) <- newBehaviour 0
    listen (value $ (*) <$> dolBh <*> ratBh) $ send tf . SetIntValue
    return $ Session (sync . dolL) (sync . ratL)

objc_interface [cunit|
@interface AppDelegate : NSResponder <NSApplicationDelegate, NSControlTextEditingDelegate>

// IBOutlets
@property (assign,nonatomic) typename NSWindow    *window;
@property (assign,nonatomic) typename NSTextField *dollars;
@property (assign,nonatomic) typename NSTextField *rate;
@property (assign,nonatomic) typename NSTextField *result;

// IBActions
- (void)controlTextDidChange:(typename NSNotification *)obj;
@end
|]

objc_implementation [Typed 'newSession, Typed 'dollarsChanged, Typed 'rateChanged]
  [cunit|

@interface AppDelegate ()
// Reference to the interpreter session in Haskell land.
@property (assign) typename HsStablePtr session;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.session = newSession(self.result);
}

- (void) controlTextDidChange:(typename NSNotification*) aNotification
{
  typename NSTextField *sender = [aNotification object];
  if ( sender == self.dollars ) {
    dollarsChanged(self.session, [self.dollars intValue]);
  } else if ( sender == self.rate ) {
    rateChanged(self.session, [self.rate intValue]);
  }
}

@end
|]

objc_emit
