data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- Define height function
height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

-- Helper function to insert an element into a binary tree
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ left val right)
    | height left <= height right =
        let newLeft = insert x left
        in balanceNode newLeft val right
    | otherwise =
        let newRight = insert x right
        in balanceNode left val newRight
  where
    -- Balances a node by updating its height and rotating if necessary
    balanceNode :: Tree a -> a -> Tree a -> Tree a
    balanceNode left val right
        | height left - height right > 1 = rotateRight left val right
        | height right - height left > 1 = rotateLeft left val right
        | otherwise = Node (max (height left) (height right) + 1) left val right

    -- Right rotation
    rotateRight :: Tree a -> a -> Tree a -> Tree a
    rotateRight (Node _ a x b) y c = Node (max (height b) (height c) + 1) a x (Node (max (height b) (height c) + 1) b y c)

    -- Left rotation
    rotateLeft :: Tree a -> a -> Tree a -> Tree a
    rotateLeft a x (Node _ b y c) = Node (max (height a) (height b) + 1) (Node (max (height a) (height b) + 1) a x b) y c

-- Main function to generate a balanced binary tree from a list of values
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Example usage
main :: IO ()
main = do
    let tree = foldTree "Abcdefghi"
    print tree
