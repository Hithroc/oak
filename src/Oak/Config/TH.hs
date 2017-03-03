{-# LANGUAGE TemplateHaskell #-}
module Oak.Config.TH where
import Language.Haskell.TH
import System.Process (readProcess)

getCommitHash :: ExpQ
getCommitHash = do
    shorthash <- runIO $ readProcess "git" ["rev-parse", "--short", "HEAD"] ""
    litE $ stringL shorthash
