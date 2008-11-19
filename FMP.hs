module FMP
      ( module FMP.Canvas
      , module FMP.Color
      , module FMP.Core
      , module FMP.File
      , module FMP.Frames
      , module FMP.Matrix
      , module FMP.Picture
      , Doc
      , module FMP.RedBlack
      , module FMP.Resolve
      , module FMP.Symbols
      , module FMP.Syntax
      , module FMP.Term
      , module FMP.Tree
      , module FMP.Turtle
      , module FMP.Types
      , metaPost, generate, funcmp
      )
  where

import Paths_funcmp (getDataDir)
import FMP.Canvas
import FMP.Color
import FMP.Core
import FMP.File
import FMP.Frames
import FMP.Matrix
import FMP.Picture
import FMP.PP
import FMP.RedBlack
import FMP.Resolve
import FMP.Symbols
import FMP.Syntax
import FMP.Term
import FMP.Tree
import FMP.Turtle
import FMP.Types
import System.Exit     ( ExitCode(..) )
import System.FilePath ( combine )
import System.Cmd      ( system )

--     Hauptkonvertierungsfunktion
--
--     ( wferi
--
--     This function is a simple wrapper containing some definitions and
--     default settings.  Starts the Nth figure with PARAMs by translating
--     picture T via |mp|.
--
--     wferi )

metaPost                      :: Int -> Picture -> Parameters -> MetaPost
metaPost n t param            =  MPVerbatim "batchmode;"
                                &  MPVerbatim (prolog param)
                                &  MPFigure n
                                      (MPAssign (Id "warningcheck") 0
                                & MPVerbatim "picture p[], q[], r[];"
                                & MPVerbatim "transform t[],tr[];"
                                & MPVerbatim "pair s[];"
                                & MPVerbatim "pair pv[][];"
                                & MPVerbatim "pair pvi[][];"
                                & MPVerbatim "numeric nv[][];"
                                & MPVerbatim "numeric nvi[][];"
                                & MPVerbatim "path tempPath;"
                                -- & MPVerbatim "pickup pencircle scaled 1;"
                                & MPAssign defDX (Const (defaultDX param))
                                & MPAssign defDY (Const (defaultDY param))
                                & MPAssign txtDX (Const (textDX param))
                                & MPAssign txtDY (Const (textDY param))
                                & l & z)
                                &  MPVerbatim (epilog param)
      where (_, _, l, z )     =  mp t (1, relax)

-- ( wferi
--
-- This is the main entry point.  Given a NAME, a NUMBER and a PICTURE it
-- |emit|s a file called NAME.MP (or NAME.NUMBER.MP in not |newmp|) with
-- a beginfig(NUMBER) in it, and runs MetaPost on it.  The conversion to
-- |MetaPost|, which |HasEmit|, is done by |metaPost|.  Finally, the
-- emitted |Doc| is |show|n.
--
-- wferi )

generate                      :: IsPicture a => String -> Int -> a -> IO ()
generate prefix n pic         =  getParameters >>= doOutput
      where
      filePath                =  prefix ++ "." ++ show n
      fileName param          =  if newmp param
                                      then prefix ++ ".mp"
                                      else filePath ++ ".mp"
      mpDoc param             =  emit (metaPost n (toPicture pic) param)
      doOutput param          =  do writeFile (fileName param)
                                      (show (mpDoc param))
                                    dataDir <- getDataDir
                                    err <- system ("MPINPUTS=${MPINPUTS}:"
                                    	           ++ combine dataDir "texmf"
                                                   ++ " " ++ mpBin param ++ " "
                                                   ++ fileName param
                                                   ++ " >> /dev/null")
                                    if err==ExitSuccess
                                      then putStr ("\\includegraphics{"
                                                   ++filePath++"}")
                                      else putStr ("Generation of picture "
                                                   ++filePath++" failed!")


funcmp                        :: IsPicture a => String -> Int -> a -> IO ()
funcmp filePath n pic         =  do param <- getParameters
                                    writeFile cacheName (show picture)
                                    system (funcmpBin param
                                              ++ " " ++ filePath
                                              ++ " " ++ show n
                                              ++ " " ++ funcmpRTS param)
                                    return ()
      where
      picture                 =  toPicture pic
      prefix                  =  filePath ++ "." ++ show n
      cacheName               =  prefix ++ ".cache"
