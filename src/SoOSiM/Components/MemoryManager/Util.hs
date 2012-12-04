module SoOSiM.Components.MemoryManager.Util where

import SoOSiM.Components.MemoryManager.Types

checkAddress ::
  [MemorySource]
  -> Int
  -> MemorySource
checkAddress sources addr = case (filter containsAddr sources) of
    []    -> error ("address unknown: " ++ show addr)
    (x:_) -> x
  where
    containsAddr (MemorySource base sc _) = base <= addr && addr < sc
