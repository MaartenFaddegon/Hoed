module Settings.Path
    (getDataDir) where

-- import Paths_Raincat_buggy(getDataDir)

--dataPath :: IO FilePath
--dataPath = getDataDir

getDataDir :: IO FilePath
getDataDir = return "../examples/Raincat"
