{-# LANGUAGE DataKinds #-}

module Frontend.HsElaborator (hsElaborate) where

import Backend.FcTypes
import Utils.Utils

hsElaborate :: FcProgram Tc -> Either String (FcProgram Fc)
hsElaborate = notImplemented "Elaborator"
