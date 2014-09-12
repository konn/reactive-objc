HC      = ghc
LIBDIR  = $(shell $(HC) --print-libdir)
CFLAGS  = -fobjc-arc -I$(LIBDIR)/include -I$(LIBDIR)/../../includes
HCFLAGS =
LDFLAGS = -package template-haskell -package language-c-quote -package language-c-inline  \
          -package sodium -package lens \
          -framework Cocoa -optl-ObjC -threaded

OBJS = Main.o App.o App_objc.o AppDelegate.o AppDelegate_objc.o
APP  = Reactive

default: $(APP).app/Contents/MacOS/$(APP)

%.o: %.hs
	$(HC) -c $< $(HCFLAGS)

AppDelegate.o:
App.o:
Main.o: App.o AppDelegate.o

App_objc.m: App.o
AppDelegate_objc.m: AppDelegate.o

$(APP): $(OBJS)
	$(HC) -o $@ $^ $(LDFLAGS)

$(APP).app:
	xcodebuild -project xcode_proj/$(APP)/$(APP).xcodeproj;
	cp -r xcode_proj/$(APP)/build/Release/$(APP).app .

$(APP).app/Contents/MacOS/$(APP): $(APP) $(APP).app
	cp $< $@

.PHONY: clean

clean:
	rm -rf $(APP).app
	rm -f *.o *.hi App_objc.[hm] AppDelegate_objc.[hm] *_stub.h $(APP)
