module Main (main) where

import qualified Cartel as C

version :: [Word]
version = [0,2]

-- Packages
base :: C.Package
base = C.nextMajor "base" [4,9,0,0]

lens :: C.Package
lens = C.atLeast "lens" [4,14]

bifunctors :: C.Package
bifunctors = C.atLeast "bifunctors" [5,3]

depends :: [C.Package]
depends =
  [ base
  , lens
  , bifunctors
  ]

props :: C.Properties
props = mempty
  { C.name = "accuerr"
  , C.version = version
  , C.cabalVersion = Just (1,10)
  , C.buildType = Just C.simple
  , C.license = Just C.bsd3
  , C.licenseFile = "LICENSE"
  , C.copyright = "2016 Omari Norman"
  , C.author = "Omari Norman"
  , C.maintainer = "omari@smileystation.com"
  , C.stability = "Experimental"
  , C.homepage = "http://www.github.com/massysett/accuerr"
  , C.bugReports = "http://www.github.com/massysett/accuerr/issues"
  , C.synopsis = "Data type like Either but with accumulating error type"
  , C.extraSourceFiles = ["README.md"]
  , C.description =
    [ "Please see the \"Accuerr\" Haddock documentation for more information." ]
  , C.category = "Development"
  }

options :: C.HasBuildInfo a => [a]
options =
  [ C.ghcOptions ["-W"]
  , C.haskell2010
  , C.hsSourceDirs ["lib"]
  , C.otherExtensions ["TemplateHaskell"]
  ]

main :: IO ()
main = C.defaultMain $ do
  libMods <- C.modules "../accuerr/lib"
  return
    ( props
    , C.exposedModules libMods
      : C.buildDepends depends
      : options
    , [ C.githubHead "massysett" "accuerr"
      ]
    )
