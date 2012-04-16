{-# LANGUAGE ScopedTypeVariables #-}
module SoOSiM.Components.Scheduler where

import Data.Maybe
import SoOSiM

import SoOSiM.Components.Scheduler.Types
import SoOSiM.Components.ResourceDiscovery.Types

scheduler schedState (ComponentMsg sender content) = do
  case (fromDynamic content) of
    Just (Execute cname memCommands) -> do
        rdCompId <- fmap fromJust $ componentLookup Nothing "ResourceDiscovery"
        --(FoundNode [nodeId]) <- fmap (fromJust . fromDynamic) $ invoke Nothing rdCompId (toDyn (FindNodes 1))
        nodeId <- createNode
        memCompId <- createComponent (Just nodeId) Nothing "MemoryManager"
        mapM_ (invokeNoWait Nothing memCompId . toDyn) memCommands
        compId <- createComponent (Just nodeId) (Just sender) cname
        invokeNoWait Nothing sender (toDyn compId)
        yield schedState
    Nothing -> yield schedState

scheduler schedState _ = yield schedState

createComponentRequest ::
  String
  -> SimM ComponentId
createComponentRequest s = do
  schedulerId    <- fmap fromJust $ componentLookup Nothing "Scheduler"
  componentIdDyn <- invoke Nothing schedulerId (toDyn s)
  return (fromJust $ fromDynamic componentIdDyn)

instance ComponentIface SchedulerState where
  initState          = SchedulerState [] []
  componentName _    = "Scheduler"
  componentBehaviour = scheduler
