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


newPosition :: Double -> Double -> Position
newPosition = Position

objc_record "" "Position" ''Position  [Typed 'newPosition]
  [ [objcprop| @property (readonly) double x; |] --> 'xCoord
  , [objcprop| @property (readonly) double y; |] --> 'yCoord
  ]
  [objcifdecls|
    + (instancetype)positionWithX:(double)x y:(double)y;
    // + (instancetype)positionWithNSPoint:(typename NSPoint)pt;
  |]
  [objcimdecls|
    + (instancetype)positionWithX:(double)x y:(double)y
    {
       return [[Position alloc] initWithPositionHsPtr: newPosition(x,y)];
    }
/*
    + (instancetype)positionWithNSPoint:(typename NSPoint)pt {
       return [[Position alloc] initWithPositionHsPtr: newPosition(pt.x,pt.y)];
    }
*/
  |]



data Size = Size { width :: Float, height :: Float
                 } deriving (Read, Show, Eq, Ord, Typeable)


newSize :: Float -> Float -> Size
newSize = Size

objc_record "My" "Size" ''Size  [Typed 'newSize]
  [ [objcprop| @property (readonly) double width; |] --> 'width
  , [objcprop| @property (readonly) double height; |] --> 'height
  ]
  [objcifdecls|
    + (instancetype)sizeWithWidth:(float)w height:(float)h;
    // + (instancetype)sizeWithNSSize:(typename NSSize)size;
  |]
  [objcimdecls|
    + (instancetype)sizeWithWidth:(float)w height:(float)h
    {
       return [[MySize alloc] initWithSizeHsPtr: newSize(w,h)];
    }
/*
    + (instancetype)sizeWithNSSize:(typename NSSize)size {
       return [[MySize alloc] initWithSizeHsPtr: newSize(size.width,size.height)];
    }
*/
  |]

newtype MySize = MySize (ForeignPtr MySize)
               deriving (Typeable)

data Rect = Rect { origin :: Position, size :: Size
                 } deriving (Read, Show, Eq, Ord, Typeable)


newRect ::  Double -> Double -> Float -> Float -> Rect
newRect x y w h = Rect (Position x y) (Size w h)

objc_record "My" "Rect" ''Rect  [Typed 'newRect]
  [ [objcprop| @property (readonly) double origX; |]
    ==> ([t| Double |], [| xCoord . origin |], [| \ p x -> p { origin = (origin p) { xCoord = x } } |])
  , [objcprop| @property (readonly) double origY; |]
    ==> ([t| Double |], [| yCoord . origin |], [| \ p y -> p { origin = (origin p) { yCoord = y } } |])
  , [objcprop| @property (readonly) float width; |]
    ==> ([t| Float |], [| width . size |], [| \ p w -> p { size = (size p) { width = w } } |])
  , [objcprop| @property (readonly) float height; |]
    ==> ([t| Float |], [| height . size |], [| \ p h -> p { size = (size p) { height = h } } |])
  ]
  [objcifdecls|
    + (instancetype)rectWithNSRect:(typename NSRect)rect;
  |]
  [objcimdecls|
    + (instancetype)rectWithNSRect:(typename NSRect)rect {
      return [[MyRect alloc] initWithRectHsPtr: newRect(rect.origin.x, rect.origin.y, rect.size.width, rect.size.height)];
    }
  |]

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
                           , definition = [cexp| newPosition([event locationInWindow].x, [event locationInWindow].y) |]
                           }

defineSelector newSelector { selector = "mouseLocation"
                           , reciever = (''NSEvent, "event")
                           , returnType = Just [t| Position |]
                           , definition = [cexp| newPosition([NSEvent mouseLocation].x, [NSEvent mouseLocation].y) |]
                           }

defineSelector newSelector { selector = "frame"
                           , reciever = (''NSWindow, "window")
                           , returnType = Just [t| Rect |]
                           , definition = [cexp| newRect([window frame].origin.x, [window frame].origin.y,
                                                         [window frame].size.width, [window frame].size.height) |]
                           }


type Listener a = a -> IO ()

data Session = Session { dollarsChanged_ :: Listener Int
                       , rateChanged_    :: Listener Int
                       , mouseMoved_     :: Listener NSEvent
                       } deriving (Typeable)


rateChanged :: Session -> Int -> IO ()
rateChanged = rateChanged_

dollarsChanged :: Session -> Int -> IO ()
dollarsChanged = dollarsChanged_

mouseMoved :: Session -> NSEvent -> IO ()
mouseMoved = mouseMoved_

objc_typecheck

nsLog :: String -> IO ()
nsLog msg = $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

newSession :: NSTextField -> NSTextField -> NSWindow -> IO Session
newSession tf lab win = sync $ do
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
    return $ Session (sync . dolL) (sync . ratL) (sync . mouseL)

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

objc_implementation [Typed 'mouseMoved, Typed 'newSession, Typed 'dollarsChanged, Typed 'rateChanged]
  [cunit|

@interface AppDelegate ()
// Reference to the interpreter session in Haskell land.
@property (assign) typename HsStablePtr session;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.session = newSession(self.result, self.mouseLabel, self.window);
  [NSEvent addGlobalMonitorForEventsMatchingMask:NSMouseMovedMask
           handler: ^(typename NSEvent *event){ mouseMoved(self.session, event); }];
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
