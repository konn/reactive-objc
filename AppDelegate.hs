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

objc_import ["<Cocoa/Cocoa.h>", "HsFFI.h"]

defineClass "NSObject"    Nothing
idMarshaller ''NSObject

defineClass "NSNotification"    (Just ''NSObject)
idMarshaller ''NSNotification

defineClass "NSString"    (Just ''NSObject)
idMarshaller ''NSString

defineClass "NSResponder"    (Just ''NSObject)
idMarshaller ''NSResponder

defineClass "NSView"   (Just ''NSResponder)
idMarshaller ''NSView

defineClass "NSWindow"   (Just ''NSResponder)
idMarshaller ''NSWindow

defineClass "NSControl"   (Just ''NSView)
idMarshaller ''NSControl

defineClass "NSTextField" (Just ''NSControl)
idMarshaller ''NSTextField


data Position = Position { xCoord :: Double, yCoord :: Double
                         } deriving (Read, Show, Eq, Ord, Typeable)


newtype MyPosition = MyPosition (ForeignPtr MyPosition)
                     deriving (Show, Eq, Ord, Typeable)

objc_typecheck

newPosition :: Double -> Double -> Position
newPosition = Position

objc_record "My" "Position" ''Position  [Typed 'newPosition]
  [ [objcprop| @property (readonly) double x; |] --> 'xCoord
  , [objcprop| @property (readonly) double y; |] --> 'yCoord
  ]
  [objcifdecls|
    + (instancetype)positionWithX:(double)x y:(double)y;
    + (instancetype)positionWithNSPoint:(typename NSPoint)pt;
  |]
  [objcimdecls|
    + (instancetype)positionWithX:(double)x y:(double)y
    {
       return [[MyPosition alloc] initWithPositionHsPtr: newPosition(x,y)];
    }
    + (instancetype)positionWithNSPoint:(typename NSPoint)pt {
       return [[MyPosition alloc] initWithPositionHsPtr: newPosition(pt.x,pt.y)];
    }
  |]

positionToMyPosition :: Position -> IO MyPosition
positionToMyPosition pos
  = $(objc ['pos :> ''Position] $
      Class ''MyPosition <: [cexp| [[MyPosition alloc] initWithPositionHsPtr: pos] |])

myPositionToPosition :: MyPosition -> IO Position
myPositionToPosition myPos
  = $(objc ['myPos :> Class ''MyPosition] $
            ''Position <: [cexp| newPosition([myPos x], [myPos y]) |])

objc_marshaller 'positionToMyPosition 'myPositionToPosition

data Size = Size { width :: Float, height :: Float
                 } deriving (Read, Show, Eq, Ord, Typeable)


newSize :: Float -> Float -> Size
newSize = Size

newtype MySize = MySize (ForeignPtr MySize)
               deriving (Typeable, Eq, Ord, Show)

objc_typecheck

objc_record "My" "Size" ''Size  [Typed 'newSize]
  [ [objcprop| @property (readonly) double width; |] --> 'width
  , [objcprop| @property (readonly) double height; |] --> 'height
  ]
  [objcifdecls|
    + (instancetype)sizeWithWidth:(float)w height:(float)h;
    + (instancetype)sizeWithNSSize:(typename NSSize)size;
  |]
  [objcimdecls|
    + (instancetype)sizeWithWidth:(float)w height:(float)h
    {
       return [[MySize alloc] initWithSizeHsPtr: newSize(w,h)];
    }
    + (instancetype)sizeWithNSSize:(typename NSSize)size {
       return [[MySize alloc] initWithSizeHsPtr: newSize(size.width,size.height)];
    }
  |]

sizeToMySize :: Size -> IO MySize
sizeToMySize size
  = $(objc ['size :> ''Size] $
      Class ''MySize <: [cexp| [[MySize alloc] initWithSizeHsPtr: size] |])

mySizeToSize :: MySize -> IO Size
mySizeToSize mySize
  = $(objc ['mySize :> Class ''MySize] $
            ''Size <: [cexp| newSize([mySize width], [mySize height]) |])

objc_marshaller 'sizeToMySize 'mySizeToSize

data Rect = Rect { origin :: Position, size :: Size
                 } deriving (Read, Show, Eq, Ord, Typeable)


newRect :: Position -> Size -> Rect
newRect = Rect

newtype MyRect = MyRect (ForeignPtr MyRect)
               deriving (Typeable, Eq, Ord, Show)

objc_typecheck

objc_record "My" "Rect" ''Rect  [Typed 'newRect]
  [ [objcprop| @property (readonly) typename MyPosition *origin; |]
    --> 'origin
  , [objcprop| @property (readonly) typename MySize *size; |]
    --> 'size
  ]
  [objcifdecls|
    + (instancetype)rectWithNSRect:(typename NSRect)rect;
  |]
  [objcimdecls|
    + (instancetype)rectWithNSRect:(typename NSRect)rect {
      return [[MyRect alloc] initWithRectHsPtr: newRect([MyPosition positionWithNSPoint: rect.origin],
                                                        [MySize sizeWithNSSize: rect.size])];
    }
  |]

rectToMyRect :: Rect -> IO MyRect
rectToMyRect rect
  = $(objc ['rect :> ''Rect] $
      Class ''MyRect <: [cexp| [[MyRect alloc] initWithRectHsPtr: rect] |])

myRectToRect :: MyRect -> IO Rect
myRectToRect myRect
  = $(objc ['myRect :> Class ''MyRect] $
            ''Rect <: [cexp| newRect([myRect origin], [myRect size]) |])

objc_marshaller 'rectToMyRect 'myRectToRect


defineSelector newSelector { selector = "intValue"
                           , reciever = (''NSControl, "ctrl")
                           , returnType = Just [t| Int |]
                           , definition = [cexp| [ctrl intValue] |]
                           }

defineSelector newSelector { selector = "setIntValue"
                           , reciever = (''NSControl, "ctrl")
                           , arguments = ["i" :>>: ''Int]
                           , definition = [cexp| [ctrl setIntValue: i] |]
                           }

defineSelector newSelector { selector = "setStringValue"
                           , reciever = (''NSControl, "ctrl")
                           , arguments = ["i" :>>: ''String]
                           , definition = [cexp| [ctrl setStringValue: i] |]
                           }

defineSelector newSelector { selector = "setFrameOrigin"
                           , reciever = (''NSView, "view")
                           , arguments = ["x" :>>: ''Double, "y" :>>: ''Double]
                           , definition = [cexp| [view setFrameOrigin: NSMakePoint(x, y) ] |]
                           }

defineClass "NSEvent" (Just ''NSObject)
idMarshaller ''NSEvent


defineSelector newSelector { selector = "locationInWindow"
                           , reciever = (''NSEvent, "event")
                           , returnType = Just [t| Position |]
                           , definition = [cexp| [MyPosition positionWithNSPoint: [event locationInWindow]] |]
                           }

defineSelector newSelector { selector = "mouseLocation"
                           , reciever = (''NSEvent, "event")
                           , returnType = Just [t| Position |]
                           , definition = [cexp| [MyPosition positionWithNSPoint: [NSEvent mouseLocation]] |]
                           }

defineSelector newSelector { selector = "frame"
                           , reciever = (''NSWindow, "window")
                           , returnType = Just [t| Rect |]
                           , definition = [cexp| [MyRect rectWithNSRect: [window frame]] |]
                           }


instance Selector "NSNotification" "object" where
  data Message "object" = NotifObject
  type Returns "object" = IO NSObject
  send' aNotif NotifObject = $(objc ['aNotif :> Class ''NSNotification] $
                               Class ''NSObject <: [cexp| [aNotif object] |])

object :: Message "object"
object = NotifObject

nsLog :: String -> IO ()
nsLog msg = $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

objc_interface [cunit|
@interface AppDelegate : NSResponder <NSApplicationDelegate, NSControlTextEditingDelegate>

// IBOutlets
@property (assign,nonatomic) typename NSWindow    *window;
@property (assign,nonatomic) typename NSTextField *dollars;
@property (assign,nonatomic) typename NSTextField *rate;
@property (assign,nonatomic) typename NSTextField *result;
@property (assign,nonatomic) typename NSTextField *mouseLabel;

// IBActions
- (void)controlTextDidChange:(typename NSNotification *)obj;
@end
|]

type Listener a = a -> IO ()

data Session = Session { dollarsChanged_ :: Listener Int
                       , rateChanged_    :: Listener Int
                       , mouseMoved_     :: Listener NSEvent
                       , application     :: AppDelegate
                       } deriving (Typeable)

rateChanged :: Session -> Int -> IO ()
rateChanged = rateChanged_

dollarsChanged :: Session -> Int -> IO ()
dollarsChanged = dollarsChanged_

mouseMoved :: Session -> NSEvent -> IO ()
mouseMoved = mouseMoved_

newtype AppDelegate = AppDelegate (ForeignPtr AppDelegate)
                      deriving (Typeable, Show, Eq, Ord)

marshalAppDelegate :: AppDelegate -> IO AppDelegate
marshalAppDelegate = return

idMarshaller ''AppDelegate

mainWindow :: AppDelegate -> IO NSWindow
mainWindow app = $(objc ['app :> ''AppDelegate] $
                   Class ''NSWindow <: [cexp| [app window] |])

dollarsField :: AppDelegate -> IO NSTextField
dollarsField app = $(objc ['app :> ''AppDelegate] $
                     Class ''NSTextField <: [cexp| [app dollars] |])

rateField :: AppDelegate -> IO NSTextField
rateField app = $(objc ['app :> ''AppDelegate] $
                  Class ''NSTextField <: [cexp| [app rate] |])

resultField :: AppDelegate -> IO NSTextField
resultField app = $(objc ['app :> ''AppDelegate] $
                  Class ''NSTextField <: [cexp| [app result] |])

mouseLabel :: AppDelegate -> IO NSTextField
mouseLabel app = $(objc ['app :> ''AppDelegate] $
                   Class ''NSTextField <: [cexp| [app mouseLabel] |])

textChanged :: Session -> NSNotification -> IO ()
textChanged sess notif = do
  sender <- notif # object
  dollars <- dollarsField $ application sess
  rate <- rateField $ application sess
  if sender === dollars
    then dollarsChanged sess =<< dollars # intValue
    else rateChanged sess =<< rate # intValue
  return ()

newSession :: AppDelegate -> IO Session
newSession app = do
  tf  <- resultField app
  lab <- mouseLabel app
  win <- mainWindow app
  sync $ do
    (dolBh, dolL) <- newBehaviour 0
    (ratBh, ratL) <- newBehaviour 0
    (mouseE, mouseL) <- newEvent
    listen (value $ (*) <$> dolBh <*> ratBh) $ send tf . setIntValue
    listen mouseE $ \ ev -> do
      Rect (Position winX winY) _ <- win # frame
      Position x y <- ev # mouseLocation
      let (relX, relY) = (x - winX, y - winY)
      lab # setStringValue (show (floor $ relX, floor $ relY))
      lab # setFrameOrigin relX relY
    return $ Session (sync . dolL) (sync . ratL) (sync . mouseL) app

objc_implementation [Typed 'textChanged, Typed 'mouseMoved, Typed 'newSession]
  [cunit|

@interface AppDelegate ()
// Reference to the interpreter session in Haskell land.
@property (assign) typename HsStablePtr session;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.session = newSession(self);
  [NSEvent addGlobalMonitorForEventsMatchingMask:NSMouseMovedMask
           handler: ^(typename NSEvent *event){ mouseMoved(self.session, event); }];
}

- (void) controlTextDidChange:(typename NSNotification*) aNotification
{
  textChanged(self.session, aNotification);
}

@end
|]

objc_emit
