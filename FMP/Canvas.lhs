--
-- FMP Canvas Module
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

> module FMP.Canvas (
>       Canvas(..),
>       cdrop, cdraw, cdraws , cfill, cfills, cclip
>       ) where

> import Numeric
> import FMP.Types
> import FMP.Picture

> data Canvas                   =  CDraw [Path]
>                               |  CClip Path
>                               |  CFill [Area]
>                               |  CDrop (Numeric, Numeric) Picture
>                               |  CConcat Canvas Canvas
>                               |  CRelax
>                                  deriving Show

> instance HasConcat Canvas where
>       (&)                     =  CConcat

> instance HasRelax Canvas where
>       relax                   =  CRelax

> instance IsPicture Canvas where
>       toPicture c             =  setTrueBoundingBox (canvas2Pic empty [c])

> canvas2Pic                    :: Picture -> [Canvas] -> Picture
> canvas2Pic p (CDraw ps:cs)    =  draw ps (canvas2Pic p cs)
> canvas2Pic p (CFill as:cs)    =  fill (map setFront as) (canvas2Pic p cs)
> canvas2Pic p (CClip path:cs)  =  clip path (canvas2Pic p cs)
> canvas2Pic p (CDrop pos p':cs)=  overlay' [ ref (1 <* C) .= vec pos ] (Just 0) [canvas2Pic p cs, p']
> canvas2Pic p (CConcat c1 c2:cs)
>                               =  canvas2Pic (canvas2Pic p cs) [c2,c1]
> --canvas2Pic p _              =  p               -- RH: original
> canvas2Pic p (CRelax:cs)      =  canvas2Pic p cs -- RH: modified
> canvas2Pic p []               =  p               -- RH: added

> cdraws                        :: IsPath a => [a] -> Canvas
> cdraws                        =  CDraw . map toPath

> cfills                        :: IsArea a => [a] -> Canvas
> cfills                        =  CFill . map toArea

> cclip                         :: IsPath a => a -> Canvas
> cclip                         =  CClip . toPath

> cdrop                         :: IsPicture a => (Numeric,Numeric) -> a -> Canvas
> cdrop at                      =  CDrop at . toPicture

> cdraw                         :: IsPath a => a -> Canvas
> cdraw a                       =  cdraws [a]

> cfill                         :: IsArea a => a -> Canvas
> cfill a                       =  cfills [a]

