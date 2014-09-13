-- HSApp: a simple Cocoa app in Haskell
--
-- Tying all components together

import qualified App         as App
import qualified AppDelegate as Delegate
import qualified Messaging   as Messaging

main :: IO ()
main
  = do
    { Messaging.objc_initialise
    ; App.objc_initialise
    ; Delegate.objc_initialise
    ; App.main
    }
