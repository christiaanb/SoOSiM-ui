-- | Contains basic operations related to the GUI
module View
  ( module View
  , module Exported
  , SimGlVar
  , SimGlSt
  )
  where

-- External libraries
import Data.CBMVar
import Graphics.UI.Gtk
import Graphics.UI.Gtk.GtkView
import Hails.MVC.View.GladeView
import Hails.MVC.View.GtkView as Exported

-- Internal libraries
import Config.Config
import Data.History
import Graphics.Diagrams.MultiCoreStatus
import Model.SystemStatus
import SoOSiM.Examples.Example1
import View.InitAnimationArea
import View.InitIconsInfoArea
import View.Tooltips
import View.Objects

instance GtkGUI View where
  initialise = createView

instance GladeView View where
  ui = uiBuilder

-- | This datatype should hold the elements that we must track in the future
-- (for instance, treeview models)
data View = View
  { uiBuilder    :: Builder
  , mcs          :: SimGlVar
  }

-- | Initialised the glade GUI and all the view components that are not
-- included directly in it
createView :: IO View
createView = do

  -- Certain aspects of the visual interface can be configured with a config file
  cfg <- readConfigFile

  bldr <- loadInterface
  ss   <- simstate 
  let emptySystemStatus = SystemStatus (historyNew emptyMultiCoreStatus) []
  msc  <- newCBMVar (emptySystemStatus, ss, initialViewState, [])

  w <- window1 bldr
  widgetShowAll w

  initialiseAnimationArea cfg msc bldr

  initIconsInfoArea bldr

  _ <- initialiseTooltips bldr

  return
    View
      { uiBuilder    = bldr
      , mcs          = msc
      }
