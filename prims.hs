import Data.List
import Data.Char

data Color = Red | Black |BBlack deriving (Eq, Read)
instance Show Color where
   show Red = "R"
   show Black = "B"
   show BBlack = "BB"

data RBtree a = Null | Node Color a (RBtree a) (RBtree a) | BBNull deriving (Show, Read, Eq)

contains :: Ord t => t -> RBtree t -> Bool
contains x (Null) = False
contains x (Node _ value left right)
   | x == value = True
   | x > value = contains x right
   | otherwise = contains x left

blackRoot :: RBtree a -> RBtree a
blackRoot (Null) = Null
blackRoot (BBNull) = Null
blackRoot (Node _ value left right) = Node Black value left right

redRedFixUp :: RBtree a -> RBtree a
redRedFixUp (Node Black value1 (Node Red value2 (Node Red value3 left3 right3) right2) right1) =
  Node Red value2 (Node Black value3 left3 right3) (Node Black value1 right2 right1) --case 1
redRedFixUp (Node Black value1 (Node Red value2 left2 (Node Red value3 left3 right3)) right1) =
   Node Red value3 (Node Black value2 left2 left3) (Node Black value1 right3 right1) -- case 2
redRedFixUp (Node Black value1 left1 (Node Red value2 (Node Red value3 left3 right3) right2)) =
   Node Red value3 (Node Black value1 left1 left3) (Node Black value2 right3 right2) -- case 3
redRedFixUp (Node Black value1 left1 (Node Red value2 left2 (Node Red value3 left3 right3))) =
   Node Red value2 (Node Black value1 left1 left2) (Node Black value3 left3 right3) -- case 4
redRedFixUp (Node BBlack value1 (Node Red value2 left2 (Node Red value3 left3 right3)) right1) =
   Node Black value3 (Node Black value2 left2 left3) (Node Black value1 right3 right1) -- case 5
redRedFixUp (Node BBlack value1 left1 (Node Red value2 (Node Red value3 left3 right3) right2) ) =
   Node Black value3 (Node Black value1 left1 left3) (Node Black value2 right3 right2) -- case 6
redRedFixUp (Node color value left right) = Node color value left right

rBinsert :: Ord a => RBtree a -> a -> RBtree a
rBinsert t x = blackRoot (rBinsert' t)
  where  rBinsert'  Null = Node Red x Null Null
         rBinsert'  (Node color a left right)
            | x == a = Node color x left right
            | x < a = redRedFixUp (Node color a (rBinsert' left)  right)
            | x > a = redRedFixUp (Node color a left  (rBinsert' right))

rBdelete :: Ord a => RBtree a -> a -> RBtree a
rBdelete t x = blackRoot (rBdelete' t)
   where rBdelete'  Null = Null
         rBdelete' (Node Red value Null Null) -- case 1
            | x == value = Null
            | otherwise = Node Red value Null Null
         rBdelete'  (Node Black value1 (Node Red value2 Null Null) Null) --case 2
            | x < value1 = (Node Black value1 (rBdelete' (Node Red value2 Null Null)) Null)
            | x == value1 = Node Black value2 Null Null
            | otherwise = (Node Black value1 (Node Red value2 Null Null) Null)
         rBdelete'  (Node Black value1 Null Null) --case 3
            | x == value1 = BBNull
            | otherwise = (Node Black value1 Null Null)
         rBdelete'  (Node color value left right)
            | x < value = blackBlackFixUp (Node color value (rBdelete' left) right)
            | x > value = blackBlackFixUp (Node color value left (rBdelete' right))
            | x == value  =  let (val , tree) = (findInordSucc right)
                              in blackBlackFixUp (Node color val left tree)

blackBlackFixUp :: RBtree a -> RBtree a
blackBlackFixUp (Node Red value1 (Node BBlack value2 left2 right2) (Node Black value3 left3 right3)) =
   redRedFixUp ((Node Black value3 (Node Red value1 (Node Black value2 left2 right2) left3 ) right3)) --case 1
blackBlackFixUp (Node Red value1 BBNull (Node black value2 left2 right2)) =
   redRedFixUp((Node Black value2 (Node Red value1 Null left2) right2)) -- case 1.1
blackBlackFixUp (Node Red value1 (Node Black value2 left2 right2) (Node BBlack value3 left3 right3)) =
   redRedFixUp(Node Black value2 left2 (Node Red value1 right2 (Node Black value3 left3 right3))) --case 2
blackBlackFixUp (Node Red value1 (Node Black value2 left2 right2) BBNull) =
   redRedFixUp (Node Black value2 left2 (Node Red value1 right2 Null)) -- case 2.1
blackBlackFixUp (Node Black value1 (Node BBlack value2 left2 right2) (Node Black value3 left3 right3)) =
   redRedFixUp(Node BBlack value3 (Node Red value1 (Node Black value2 left2 right2) left3) right3) --case 3
blackBlackFixUp (Node Black value1 BBNull (Node Black value2 left2 right2)) =
   redRedFixUp(Node BBlack value2 (Node Red value1 Null left2) right2) -- case 3.1
blackBlackFixUp (Node Black value1 (Node Black value2 left2 right2) (Node BBlack value3 left3 right3)) =
   redRedFixUp(Node BBlack value2 left2 (Node Red value1 right2 (Node Black value3 left3 right3))) --case 4
blackBlackFixUp (Node Black value1  (Node Black value2 left2 right2) BBNull) =
   redRedFixUp(Node BBlack value2 left2 (Node Red value1 right2 Null)) -- case 4.1
blackBlackFixUp (Node Black value1 (Node BBlack value2 left2 right2) (Node Red value3 (Node Black value4 left4 right4) right3)) =
   (Node Black value3 (redRedFixUp(Node Black value4 (Node Red value1 (Node Black value2 left2 right2) left4) right4)) right3) --case 5
blackBlackFixUp (Node Black value1 BBNull (Node Red value3 (Node Black value4 left4 right4) right3)) =
   (Node Black value3 (redRedFixUp(Node Black value4 (Node Red value1 Null left4) right4)) right3) --case 5.1
blackBlackFixUp (Node Black value1 (Node Red value2 left2 (Node Black value3 left3 right3)) (Node BBlack value4 left4 right4)) =
   (Node Black value2 left2 (redRedFixUp(Node Black value3 left3 (Node Red value1 right3 (Node Black value4 left4 right4))))) -- case 6
blackBlackFixUp (Node Black value1 (Node Red value2 left2 (Node Black value3 left3 right3)) BBNull) =
   (Node Black value2 left2 (redRedFixUp(Node Black value3 left3 (Node Red value1 right3 Null)))) -- case 6.1
blackBlackFixUp (Node color value left right) = Node color value left right

findInordSucc :: RBtree a -> (a,RBtree a)
findInordSucc (Node Red value Null Null) = (value, Null)
findInordSucc (Node Black value Null Null) = (value, BBNull)
findInordSucc (Node Black value Null (Node Red value2 Null Null)) = (value, Node Black value2 Null Null)
findInordSucc (Node color value left right) = let (val, tree) = findInordSucc left
                                              in (val, blackBlackFixUp (Node color value tree right))

findMin :: RBtree (a,b,c) -> (a,b,c)
findMin (Node c (v,ve,prev) Null Null) = (v,ve,prev)
findMin (Node c (v,ve,prev) Null right) = (v,ve,prev)
findMin (Node c (v,ve,prev) left right) = findMin left

fromJust :: Maybe a -> a
fromJust (Just a) = a

strToEdges :: [Char] -> [(Int,Int,Int)]
strToEdges line = [((read id1),(read weight),(read id2)),((read id2),(read weight),(read id1))]
          where (id1:id2:weight:xs) = words line

createAdjList :: Foldable t => t [Char] -> [(Int,[(Int,Int)])]
createAdjList listofLines = [(id1,(edgesFrom id1)) | id1 <- allVertices ]
    where listofEdges = concatMap strToEdges listofLines
          allVertices = nub (map extractid1 listofEdges)
          edgesFrom id1 = [(weight,id2) | (id11,weight,id2) <- listofEdges, id1 == id11]
          extractid1 (id1,weight,id2) = id1

addvalues :: (Ord a, Ord b, Ord t) => RBtree (a,b,t) -> [(a,b)] -> t -> RBtree (a,b,t)
addvalues tree [] currVertex = tree
addvalues tree ((weight, vertex):xs) currVertex = addvalues (rBinsert tree (weight,vertex,currVertex)) xs currVertex

startPrim :: (Ord a, Ord b, Num a) => [(b, [(a,b)])] -> [(b,b)]
startPrim ((id1,listofEdges):xs) = delete (id1,id1) (startPrim' Null ((id1,listofEdges):xs) initrbTree [])
    where initrbTree = rBinsert Null (0,id1,id1)

startPrim' :: (Ord a, Ord b) => RBtree b -> [(b,[(a,b)])] -> RBtree(a,b,b) -> [(b,b)] -> [(b,b)]
startPrim' treeforV adjList Null output = output
startPrim' treeforV adjList tree output
   | (contains currVertex treeforV) == True = startPrim' treeforV adjList (tree') output
   | (contains currVertex treeforV) == False = startPrim' updTreeV adjList updTreeQue ( (currPrec,currVertex):output)
        where (currWeight, currVertex, currPrec) = findMin tree
              tree' = rBdelete tree (currWeight,currVertex,currPrec)
              currList = fromJust ((lookup currVertex adjList))
              updTreeV = (rBinsert treeforV currVertex)
              updTreeQue = rBdelete subTree (currWeight,currVertex,currPrec)
              subTree = addvalues tree currList currVertex

finalPrim :: Foldable t => t [Char] -> [(Int,Int)]
finalPrim listofLines = startPrim (createAdjList listofLines)

reader :: IO [String]
reader = do
    input <- getContents
    return (lines input)

updPrinter :: (Show a1, Show a2) => (a1, a2) -> IO ()
updPrinter (id1,id2) = putStrLn ((show id1) ++ " " ++ (show id2))

main = do
  xs <- reader
  mapM updPrinter (finalPrim xs)
