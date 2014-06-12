-- Why is there no package of this..
module Plugin.Yard.Stack where

type Stack = [] 

isEmpty :: Stack a -> Bool
isEmpty = null

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> (a, Stack a)
pop (a:s) = (a,s)
pop [] = error "Can't pop from empty Stack"

peek :: Stack a -> a
peek (a:_) = a
peek [] = error "Can't peek on empty Stack"