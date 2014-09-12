#import <Cocoa/Cocoa.h>
#include "HsFFI.h"
#import "MyBridge.h"
HsStablePtr MyBridge(id a) {
  return (__bridge HsStablePtr) a;
}
