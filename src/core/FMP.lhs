-- 
-- FMP Toplevel Module
-- Copyright (C) 1998 Joachim Korittky
-- 
-- This file is part of Functional MetaPost.
-- 
-- Functional MetaPost is free software; you can redistribute it
-- and/or modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- Functional MetaPost is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with Functional MetaPost; if not, write to the
-- Free Software Foundation, Inc., 59 Temple Place, Suite 330,
-- Boston, MA 02111-1307 USA
-- 

> module FMP(
>	metaPost, generate, funcmp,
>	) where 

> import System		(system, ExitCode(..))
> import FMPPicture	(Picture(), IsPicture(..),stdAttrib,HasConcat(..))
> import FMPMpsyntax	(MetaPost(..),Term(..),defDX,defDY,txtDX,txtDY,emit)
> import FMPSymbols	
> import FMPCore	(mp)
> import FMPFile	(Parameters(..),clearFile,getParameters)

Hauptkonvertierungsfunktion

( wferi

This function is a simple wrapper containing some definitions and
default settings.  Starts the Nth figure with PARAMs by translating
picture T via |mp|.

wferi )

> metaPost 			:: Int -> Picture -> Parameters -> MetaPost
> metaPost n t param		=  MPVerbatim "batchmode;"
>				  &  MPVerbatim (prolog param)
>				  &  MPFigure n
>				        (MPAssign (Id "warningcheck") 0
>					& MPVerbatim "picture p[], q[], r[];"
>					& MPVerbatim "transform t[],tr[];"
>					& MPVerbatim "pair s[];"
>					& MPVerbatim "pair pv[][];"
>					& MPVerbatim "pair pvi[][];"
>					& MPVerbatim "numeric nv[][];"
>					& MPVerbatim "numeric nvi[][];"
>					& MPVerbatim "path tempPath;"
>					-- & MPVerbatim "pickup pencircle scaled 1;"
>					& MPAssign defDX (Const (defaultDX param))	---
>					& MPAssign defDY (Const (defaultDY param))	---
>					& MPAssign txtDX (Const (textDX param))
>					& MPAssign txtDY (Const (textDY param))
>					& l & z)
>				  &  MPVerbatim (epilog param)
>	where (_, _, l, z )	=  mp t (1, relax)

( wferi

This is the main entry point.  Given a NAME, a NUMBER and a PICTURE it
|emit|s a file called NAME.MP (or NAME.NUMBER.MP in not |newmp|) with
a beginfig(NUMBER) in it, and runs MetaPost on it.  The conversion to
|MetaPost|, which |HasEmit|, is done by |metaPost|.  Finally, the
emitted |Doc| is |show|n.

wferi )

> generate			:: IsPicture a => String -> Int -> a -> IO ()
> generate prefix n pic		=  getParameters >>= doOutput
>	where
>	filePath		=  prefix ++ "." ++ show n
>	fileName param		=  if newmp param
>					then prefix ++ ".mp"
>					else filePath ++ ".mp"
>	mpDoc param		=  emit (metaPost n (toPicture pic) param)
>	doOutput param  	=  do writeFile (fileName param)
>					(show (mpDoc param))
>		                      err <- system (mpBin param ++ " "
>						     ++ fileName param
>						     ++ " >> /dev/null")
>			              if err==ExitSuccess
>					then putStr ("\\includegraphics{"
>						     ++filePath++"}")
>					else putStr ("Generation of picture "
>						     ++filePath++" failed!")


> funcmp			:: IsPicture a => String -> Int -> a -> IO ()
> funcmp filePath n pic		=  do param <- getParameters
>				      writeFile cacheName (show picture)
>				      err <- system (funcmpBin param
>						++ " " ++ filePath
>						++ " " ++ show n 
>						++ " " ++ funcmpRTS param)
>				      return ()
>	where
>	picture			=  toPicture pic
>	prefix			=  filePath ++ "." ++ show n
>	cacheName		=  prefix ++ ".cache"
