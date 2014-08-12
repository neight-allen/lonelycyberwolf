module Actor.Conductor.Implementation
    ( SheetMusic
    , conductor
    ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Platform.ManagedProcess

import           Actor.Conductor
import           Actor.Types

data SheetMusic = SheetMusic

----

conductor :: ProcessDefinition SheetMusic
conductor = ProcessDefinition
        { apiHandlers            = []
        , infoHandlers           = []
        , exitHandlers           = []
        , timeoutHandler         = \s _ -> continue s
        , shutdownHandler        = \_ _ -> return ()
        , unhandledMessagePolicy = Drop
        }
