{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
module Raspberry.IP
  ( Host, Port, WebAddress (..)
  ) where
--------------------------------------------------------------------------------
import Import.DeriveJSON
--------------------------------------------------------------------------------

type Host = String
type Port = Int

data WebAddress = WebAddress { host :: Host, port :: Port } deriving (Show, Eq)
deriveJSON jsonOptions ''WebAddress
