--
-- FMP Command-Line Interface
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

> module Main where
> import System
> import IO
> import FMP.PP
> import FMP.Picture    hiding (text, empty, space)
> import FMP.Syntax
> import FMP.File
> import FMP

> main                          :: IO ()
> main                          =  getArgs
>                               >>= \args
>                               -> let filePath = head args in
>                               let num         = head (tail args) in
>                               readFile (filePath ++ "." ++ num ++ ".cache")
>                               >>= \file
>                               -> let fileName         = filePath ++ "."
>                                                       ++ num ++ ".mp" in
>                                       do param <- getParameters
>                                          writeFile fileName
>                                               (show (mpDoc num file param))
>                                          err   <- system (mpBin param ++ " "
>                                                               ++ fileName
>                                                               ++ " >> /dev/null")
>                                          if err == ExitSuccess
>                                               then putStr ("\\includegraphics{"
>                                                       ++ filePath ++ "." ++ num ++ "}")
>                                                 >> return ()
>                                               else putStr "Error"
>                                                 >> return ()
>       where
>       mpDoc num file param    =  case [x | (x, t) <- reads file, ("", "") <- lex t] of
>                       [x]     -> emit (metaPost (read num) x param)
>                       _       -> text "end"


