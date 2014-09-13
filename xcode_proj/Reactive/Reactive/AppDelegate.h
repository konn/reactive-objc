//
//  AppDelegate.h
//  Reactive
//
//  Created by 石井 大海 on 2014/09/12.
//  Copyright (c) 2014年 Hiromi ISHII. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (assign) IBOutlet NSWindow *window;
@property (assign) IBOutlet NSTextField *dollars;
@property (assign) IBOutlet NSTextField *rate;
@property (assign) IBOutlet NSTextField *result;
@property (assign) IBOutlet NSTextField *mouseLabel;

- (IBAction)controlTextDidChange:(NSNotification *)obj;
@end
