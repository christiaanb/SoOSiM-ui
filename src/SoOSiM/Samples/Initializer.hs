-- | A sample application with just an initializer
module SoOSiM.Samples.Initializer where

-- -- External imports
-- import qualified Data.IntMap as IM
-- import qualified Data.Map as Map
-- import SoOSiM.Types
-- import UniqSupply
-- import Unique
-- 
-- -- Local imports
-- import SoOSiM.Components.Initializer
-- 
-- simstate :: IO SimState
-- simstate = do
--   supply <- mkSplitUniqSupply 'z'
--   let (supply',supply'')       = splitUniqSupply supply
--       (node0id:component0id:_) = uniqsFromSupply supply'
-- 
--       -- Main component and node
--       component0CC = CC Running Initializer (error "no parent") [Initialize]
--       node0        = Node node0id NodeInfo
--                           (Map.fromList [("Initializer",component0id)])
--                           (IM.fromList [(getKey component0id,component0CC)])
--                           IM.empty
-- 
--       -- Initial state
--       simState = SimState node0id component0id
--                           (IM.fromList [(getKey node0id,node0)])
--                           supply''
--                           (Map.fromList [("Initializer",component0CC)])
-- 
--   return simState

import Control.Concurrent.STM
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map    as Map
import SoOSiM
import SoOSiM.Simulator
import SoOSiM.Types
import UniqSupply
import Unique
-- import Text.PrettyPrint.HughesPJ

import SoOSiM.Components.HeatMap.Application
import SoOSiM.Components.HeatMap.Types

import SoOSiM.Components.MemoryManager
import SoOSiM.Components.MemoryManager.Types

simstate :: IO SimState
simstate = do
    supply <- mkSplitUniqSupply 'z'
    let (supply',supply'')       = splitUniqSupply supply
    let (node0id:component0id:_) = uniqsFromSupply supply'
    statusTV <- newTVarIO Running
    stateTV  <- newTVarIO Initializer
    bufferTV <- newTVarIO [Initialize]
    let component0CC             = CC component0id statusTV stateTV component0id bufferTV
    let node0                    = Node node0id NodeInfo Map.empty (IM.fromList [(getKey component0id,component0CC)]) IM.empty []
    let simState                 = SimState node0id component0id (IM.fromList [(getKey node0id,node0)]) supply'' Map.empty
    return simState

--     loop 0 simState
--     return ()
--   where
--     loop ::
--       Int
--       -> SimState
--       -> IO ()
--     loop n simState = do
--       putStrLn $ "Cycle: " ++ show n
--       (fmap render $ showIO simState) >>= putStrLn
--       simState' <- execStep simState
--       c <- getChar
--       case c of
--         'n' -> loop (n+1) simState'
--         _   -> return ()

data Initializer = Initializer

initializer ::
  Initializer
  -> ComponentInput
  -> SimM Initializer
initializer s Initialize = do
  nId <- getNodeId
  registerComponent (initState :: MemState)
  registerComponent (initState :: HMState)
  _ <- createComponent (Just nId) Nothing "MemoryManager"
  _ <- createComponent (Just nId) Nothing "HeatMap"
  yield s

initializer s _ = yield s

instance ComponentIface Initializer where
  initState          = Initializer
  componentName _    = "Initializer"
  componentBehaviour = initializer