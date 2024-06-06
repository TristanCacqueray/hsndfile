{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provides some examples for soundfile I\/O with 'Sound.File.Sndfile.Buffer.Vector.Buffer'. Click the /Source/ links to access the source code.
module Sound.File.Sndfile.Examples where

import qualified Data.Vector.Storable as SV
import qualified Sound.File.Sndfile as SF

-- | Read a sound file, normalize the contents and write it back.
--
-- The file is read into memory in its entirety, which may not be feasible for large files. No deinterleaving is needed in this case.
normalizeSoundFile :: FilePath -> FilePath -> IO ()
normalizeSoundFile inPath outPath = do
    (info, Just (samples :: SV.Vector Double)) <- SF.readFile inPath
    let n = SV.maximum (SV.map abs samples)
        y = if n == 0 then samples else SV.map (/n) samples
    _ <- SF.writeFile info outPath y
    return ()
