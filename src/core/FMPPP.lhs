-- 
-- FMP Pretty Printer Module
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

> module FMPPP (
> 	Doc,
> 	empty, text, char, double, int,
> 	parens, brackets, semi, comma, colon, space, equals,
> 	lparen, rparen, lbrack, rbrack,
>       quotes, doubleQuotes,
> 	(<>), (<+>), hcat, hsep, 
> 	($$), ($+$), vcat, 
> 	punctuate,
>	) where
 
 
> infixl 6 <> 
> infixl 6 <+>
> infixl 5 $$, $+$

> data Doc			=  Empty
>  				|  Doc `Beside` Doc
>  				|  Text String
>  				|  Return Doc
 
> instance Show Doc where
>	showsPrec _ doc cont	= fst (showDoc doc 0 cont)
 
> semi,colon,comma,space,equals	:: Doc
> semi				= char ';'
> colon				= char ':'
> comma				= char ','
> space				= char ' '
> equals			= char '='

> lparen,rparen,lbrack,rbrack	:: Doc
> lparen			= char '('
> rparen			= char ')'
> lbrack			= char '['
> rbrack			= char ']'

> empty				:: Doc
> empty				=  Empty
> int				:: Show a => a -> Doc
> int n				=  text (show n)
> char				:: Char -> Doc
> char c			=  Text [c]
> text				:: String -> Doc
> text s			=  Text s
> parens			:: Doc -> Doc
> parens p			=  char '(' <> p <> char ')'

> brackets			:: Doc -> Doc
> brackets p			=  char '[' <> p <> char ']'

> quotes                        :: Doc -> Doc
> quotes p                      =  char '\'' <> p <> char '\''

> doubleQuotes                  :: Doc -> Doc
> doubleQuotes p                =  char '\"' <> p <> char '\"'                         

> double			:: Show a => a -> Doc
> double n			=  text (show n)
 
> hcat,hsep,vcat		:: [Doc] -> Doc
> hcat				=  foldr (<>)  empty
> hsep				=  foldr (<+>) empty
> vcat				=  foldr ($$)  empty
 
> punctuate			:: Doc -> [Doc] -> [Doc]
> punctuate _ []		=  []
> punctuate p (d:ds)		=  go d ds
>		where
> 		go d []		=  [d]
> 		go d (e:es)	=  (d <> p) : go e es
 
> ($$)				:: Doc -> Doc -> Doc
> p $$  q			=  p `Beside` Return q

> ($+$)				:: Doc -> Doc -> Doc
> p $+$ q			=  p `Beside` Return q
 
> (<>)				:: Doc -> Doc -> Doc
> p <>  q			=  p `Beside` q

> (<+>)				:: Doc -> Doc -> Doc
> p <+> q			=  (p `Beside` Text " ") `Beside` q


> showDoc			:: Doc -> Int -> String -> (String, Int)
> showDoc (d1 `Beside` d2) n c	= (d1', n'')
>	where	(d1', n')	=  showDoc d1 n  d2'
>		(d2', n'')	=  showDoc d2 n' c
> showDoc (Text s) n cont	=  if n > 100
>					then (showString ('\n':s) cont, 0)
>					else (showString s cont, n+1)
> showDoc (Return d) n c	=  (showString "\n" d',n')
>	where	(d',n')		=  showDoc d 0 c
> showDoc Empty n cont		=  (cont, n)
