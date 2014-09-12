{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell #-}
module AppDelegate (objc_initialise) where

  -- language-c-inline
import Data.Typeable          (Typeable)
import FRP.Sodium
import Language.C.Inline.ObjC
import Language.C.Quote.ObjC

  -- friends

objc_import ["<Cocoa/Cocoa.h>", "MyBridge.h"]

type Listener a = a -> IO ()

data Session = Session { onClick_       :: Listener ()
                       , onTextChanged_ :: Listener String
                       } deriving (Typeable)

onClick :: Session -> () -> IO ()
onClick = onClick_

onTextChanged :: Session -> String -> IO ()
onTextChanged = onTextChanged_

newtype NSTextField = NSTextField (ForeignPtr NSTextField)
                      deriving (Typeable)

objc_typecheck

marshTF :: NSTextField -> IO NSTextField
marshTF = return

objc_marshaller 'marshTF 'marshTF

nsLog :: String -> IO ()
nsLog msg = $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

newSession :: NSTextField -> IO Session
newSession tf = sync $ do
    (clkEv, clkL) <- newEvent
    (txtBh, txtL) <- newBehaviour ""
    listen clkEv $ \() -> $(objc ['tf :> Class ''NSTextField] $
                            void $ [cexp| [tf setStringValue: @"Clicked!"] |])
    listen (value txtBh) $ \val ->
       $(objc ['val :> ''String, 'tf :> Class ''NSTextField] $
              void $ [cexp| [tf setStringValue: val] |])
    return $ Session (sync . clkL) (sync . txtL)

objc_interface [cunit|
@interface AppDelegate : NSResponder <NSApplicationDelegate, NSControlTextEditingDelegate>

// IBOutlets
@property (weak, nonatomic) typename NSWindow    *window;
@property (assign, nonatomic) typename NSTextField *notif;
// IBActions
- (typename IBAction) clicked:(id) sender;
- (void) controlTextDidChange:(typename NSNotification*) aNotification;
@end
|]

objc_implementation [Typed 'newSession, Typed 'onClick, Typed 'onTextChanged]
  [cunit|

@interface AppDelegate ()
// Reference to the interpreter session in Haskell land.
@property (assign) typename HsStablePtr session;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  NSLog(@"hello!");
  self.session = newSession(self.notif);
  NSLog(@"session initialized.");
}

- (void) controlTextDidChange:(typename NSNotification*) aNotification
{
  onTextChanged(self.session, [[[aNotification userInfo] objectForKey: @"NSFieldEditor"] string]);
}

// IBAction
- (void) clicked: (id) sender
{
  onClick(self.session, NULL);
}
@end
|]

objc_emit
