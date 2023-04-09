module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes graph = case graph of
    Empty -> S.empty
    Node a -> S.fromList [a]
    Overlay a b -> S.union (nodes a) (nodes b)
    Connect a b -> S.union (nodes a) (nodes b)

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges graph = case graph of
    Empty -> S.empty
    Node a -> S.empty
    Overlay a b -> S.union (edges a) (edges b)
    Connect a b -> S.union (S.cartesianProduct (nodes a) (nodes b)) (S.union (edges a) (edges b))

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node graph = case graph of
    Node a -> S.empty
    Connect a b -> if S.member node (nodes a) then 
                    S.union (nodes b) (outNeighbors node a)
                    else (outNeighbors node b)
    Overlay a b -> S.union (outNeighbors node a) (outNeighbors node b)

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node graph = case graph of
    Node a -> S.empty
    Connect a b -> if S.member node (nodes b) then 
                    S.union (nodes a) (inNeighbors node b)
                    else (inNeighbors node a)
    Overlay a b -> S.union (inNeighbors node a) (inNeighbors node b)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph = case graph of
    Empty -> Empty
    Node a -> if node == a then
                Empty
                else Node a
    Connect a b -> Connect (removeNode node a) (removeNode node b)
    Overlay a b -> Overlay (removeNode node a) (removeNode node b)

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news graph = case graph of
    Empty -> Empty
    Node a -> if old == a then
                helperSplitNode news (length news)
                else Node a
    Overlay a b -> Overlay (splitNode old news a) (splitNode old news b)
    Connect a b -> Connect (splitNode old news a) (splitNode old news b)

helperSplitNode :: Eq a
                => [a]
                -> Int
                -> AlgebraicGraph a
helperSplitNode news size = if size == 0 then
                            Empty
                            else Overlay (Node (head news)) (helperSplitNode (tail news) (size - 1)) 

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = case graph of
    Empty -> Empty
    Node a -> if prop a then
                Node node
                else Node a
    Overlay a b -> Overlay (mergeNodes prop node a) (mergeNodes prop node b)
    Connect a b -> Connect (mergeNodes prop node a) (mergeNodes prop node b)
