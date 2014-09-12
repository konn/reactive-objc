HC      = ghc
LIBDIR  = $(shell $(HC) --print-libdir)
CFLAGS  = -fobjc-arc -I$(LIBDIR)/include -I$(LIBDIR)/../../includes
HCFLAGS =
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline  \
          -package sodium -package lens \
          -framework Cocoa -optl-ObjC -threaded

OBJS = Main.o App.o App_objc.o AppDelegate.o AppDelegate_objc.o MyBridge.o

default: Reactive.app/Contents/MacOS/Reactive

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

AppDelegate.o: MyBridge.o
App.o:
Main.o: App.o AppDelegate.o
MyBridge.o:

App_objc.m: App.o
AppDelegate_objc.m: AppDelegate.o

Reactive: $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

Reactive.app:
	xcodebuild -project xcode_proj/Reactive/Reactive.xcodeproj;
	cp -r xcode_proj/Reactive/build/Release/Reactive.app .

Reactive.app/Contents/MacOS/Reactive: Reactive Reactive.app
	cp $< $@

.PHONY: clean

clean:
	rm -rf Reactive.app
	rm -f *.o *.hi App_objc.[hm] AppDelegate_objc.[hm] *_stub.h Reactive
