module FMP.RedBlack where
import FMP.Types
import FMP.Picture
import FMP.Color
import FMP.Tree
import FMP.Frames

data Two34 a                  =  Nil
                              |  Two   (Two34 a) a (Two34 a)
                              |  Three (Two34 a) a (Two34 a) a (Two34 a)
                              |  Four  (Two34 a) a (Two34 a) a (Two34 a)
                                      a (Two34 a)
                                 deriving (Show)

-- Beispiel:
--
-- rbtree                        :: Two34 String
-- rbtree                        =  Four (Four Nil "A" Nil "A" Nil "C" Nil)
--                                       "E"
--                                       (Three Nil "G" Nil "H" Nil)
--                                       "I"
--                                       (Two Nil "N" Nil)
--                                       "R"
--                                       (Two Nil "S" Nil)
--
-- \begin{center}
-- \perform{ generate "rs" 1 bsp }
-- \end{center}
--
-- \vspace{1em}
--
-- bsp = transform (scaled 0.6) (toPicture (forEachPic (setBGColor (grey 0.9))
--               (forEachEdge (setPen 1) (convert234 rbtree))))
--  |||| transform (scaled 0.6) (toPicture (forEachPic (setBGColor (grey 0.9))
--               (forEachEdge (setPen 1) (convertRS rbtree))))

-- Konvertierung eines 234-Baumes in den Picture-Typ.

edgeN                         :: Show a => a -> Tree -> Edge
edgeN n                       =  edge' (line  (ref (This <+ C))
                                              (ref (Parent <+ ('p':show n) <+ C)))
edgeNRed                      :: Show a => a -> Tree -> Edge
edgeNRed n t                  =  edgeN n t
                                 # setPattern dashed
                                 # setColor red

tiny                          :: String -> Picture
tiny a                        =  tex ("\\tiny "++a)

tbox s                        =  oval (   (dot # setName "p1")
                                      ||| tiny s
                                      ||| (dot # setName "p2"))

tbox2 s1 s2                   =  oval (   (dot # setName "p1")
                                      ||| tiny s1
                                      ||| (dot # setName "p2")
                                      ||| tiny s2
                                      ||| (dot # setName "p3"))

tbox3 s1 s2 s3                =  oval (   (dot # setName "p1")
                                      ||| tiny s1
                                      ||| (dot # setName "p2")
                                      ||| tiny s2
                                      ||| (dot # setName "p3")
                                      ||| tiny s3
                                      ||| (dot # setName "p4"))

convert234                    :: Two34 String -> Tree
convert234 Nil                =  node (box empty) []
convert234 (Two t1 a t2)      =  node (tbox a)
                                      [edgeN 1 (convert234 t1),
                                       edgeN 2 (convert234 t2)]
convert234 (Three t1 a1 t2 a2 t3)
                              =  node (tbox2 a1 a2)
                                      [edgeN 1 (convert234 t1),
                                       edgeN 2 (convert234 t2),
                                       edgeN 3 (convert234 t3)]
convert234 (Four t1 a1 t2 a2 t3 a3 t4)
                              =  node (tbox3 a1 a2 a3)
                                      [edgeN 1 (convert234 t1),
                                       edgeN 2 (convert234 t2),
                                       edgeN 3 (convert234 t3),
                                       edgeN 4 (convert234 t4)]

-- Konvertierung eines 234-Baumes in den Picture-Typ der Rot-schwarz-darstellung.
-- Der Konstruktur Three' erzeugt die alternative Darstellung des Dreierknotens.

convertRS                     :: Two34 String -> Tree
convertRS Nil                 =  node (box empty) []
convertRS (Two t1 a t2)       =  node (tbox a)
                                      [edgeN 1 (convertRS t1),
                                       edgeN 2 (convertRS t2)]
convertRS (Three t1 a1 t2 a2 t3)
                              =  node (tbox a1)
                                      [edgeNRed 1 (node (tbox a2)
                                              [edgeN 1 (convertRS t1),
                                               edgeN 2 (convertRS t2)]),
                                       edgeN 2 (convertRS t3)]
convertRS (Four t1 a1 t2 a2 t3 a3 t4)
                              =  node (tbox a1)
                                      [edgeNRed 1 (node (tbox a2)
                                              [edgeN 1 (convertRS t1),
                                               edgeN 2 (convertRS t2)]),
                                       edgeNRed 2 (node (tbox a3)
                                              [edgeN 1 (convertRS t3),
                                               edgeN 2 (convertRS t4)])
                                      ]
