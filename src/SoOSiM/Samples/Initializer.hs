{-# LANGUAGE TypeFamilies #-}
-- | A sample application with just an initializer
module SoOSiM.Samples.Initializer where

import           Control.Concurrent.Supply
import           Control.Concurrent.STM
import           Data.Maybe
import qualified Data.IntMap                as IM
import qualified Data.Map                   as Map
import           SoOSiM
import           SoOSiM.Types

import SoOSiM.Components.HeatMap.Application
import SoOSiM.Components.HeatMap.Types

import SoOSiM.Components.MemoryManager
import SoOSiM.Components.MemoryManager.Types

import SoOSiM.Components.Scheduler
import SoOSiM.Components.Scheduler.Types

simstate :: IO SimState
simstate = do
    supply <- newSupply
    let (node0id,supply')       = freshId supply
    let (component0id,supply'') = freshId supply'
    statusTV <- newTVarIO ReadyToRun
    stateTV  <- newTVarIO ()
    bufferTV <- newTVarIO [Tick]
    let emptyMeta = SimMetaData 0 0 0 Map.empty Map.empty
    emptyMetaTV   <- newTVarIO emptyMeta
    let component0CC = CC Initializer component0id component0id statusTV
                        stateTV bufferTV [] emptyMetaTV
    let node0 = Node node0id NodeInfo Map.empty
                  (IM.fromList [(component0id,component0CC)])
                  IM.empty [component0id]
    let simState = SimState node0id component0id
                    (IM.fromList [(node0id,node0)]) supply''
    return simState

data Initializer = Initializer

initializer ::
  ()
  -> Input ()
  -> Sim ()
initializer s Tick = do
  _ <- createComponent MemoryManager
  _ <- createComponent Scheduler
  hmId <- createComponent HeatMap
  invokeAsync HeatMap hmId Compute ignore
  yield s

initializer s _ = yield s

instance ComponentInterface Initializer where
  type Receive Initializer = ()
  type Send Initializer    = ()
  type State Initializer   = ()
  initState _          = ()
  componentName _      = "Initializer"
  componentBehaviour _ = initializer
