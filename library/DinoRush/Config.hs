module DinoRush.Config
  ( Config(..)
  , Resources(..)
  ) where

import qualified SDL

import DinoRush.Resource

data Config = Config
  { cWindow :: SDL.Window
  , cRenderer :: SDL.Renderer
  , cResources :: Resources
  }
