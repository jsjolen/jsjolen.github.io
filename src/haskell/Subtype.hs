{-# LANGUAGE StandaloneDeriving #-}
import qualified Data.Set as Set
import Debug.Trace

data StructuralType =
  Union [StructuralType] Integer
  | Record [StructuralType] Integer
  | Int Integer
  | Null Integer
deriving instance Eq StructuralTypeb
deriving instance Ord StructuralType

-- Avoid infinite recursion by not inspecting middle argument.
instance Show StructuralType where
    show (Union _ id) = "Union " ++ (show id)
    show (Record _ id) = "Record " ++ (show id)
    show (Int id) = "Int " ++ (show id)
    show (Null id) = "Null " ++ (show id)

intt = Int 0
nullt = Null 1
linkedList =
  let node = Union [nullt, Record [intt, node] 3] 2
  in node
inode = Union [nullt, Record [intt, onode] 5] 4
onode = Union [nullt, Record [intt, inode] 7] 6
innerList = inode
outerList = onode

tmember :: StructuralType -> Set.Set Integer -> Bool
tmember (Record _ id) set = Set.member id set
tmember (Union _ id) set = Set.member id set
tmember (Int id) set = Set.member id set
tmember (Null id) set = Set.member id set

tinsert :: StructuralType -> Set.Set Integer -> Set.Set Integer
tinsert (Record _ id) set = Set.insert id set
tinsert (Union _ id) set = Set.insert id set
tinsert (Int id) set = Set.insert id set
tinsert (Null id) set = Set.insert id set

-- Requires identical ordering of elements
isSubtypeOf :: StructuralType -> StructuralType -> Set.Set Integer -> Set.Set Integer -> (Bool, Set.Set Integer, Set.Set Integer)
isSubtypeOf (Union tsA i) (Union tsB i') seenA seenB =
  foldl (\(subtype, seenA', seenB') (a,b) ->
           if (tmember a seenA' && tmember b seenB') then (subtype, seenA', seenB') else
             let (subtype', seenA'', seenB'') = isSubtypeOf a b seenA' seenB'
             in (subtype' && subtype, seenA'', seenB''))
        (True,Set.insert i seenA, Set.insert i' seenB) $ zip tsA tsB
isSubtypeOf (Record tsA i) (Record tsB i') seenA seenB =
  foldl (\(subtype, seenA', seenB') (a,b) ->
           if (tmember a seenA' && tmember b seenB') then (subtype, seenA', seenB') else
             let (subtype', seenA'', seenB'') = isSubtypeOf a b seenA' seenB'
             in (subtype' && subtype, seenA'', seenB''))
        (True,Set.insert i seenA, Set.insert i' seenB) $ zip tsA tsB
isSubtypeOf (Int ida) (Int idb) seenA seenB =
  let seenA' = Set.insert ida seenA
      seenB' = Set.insert idb seenB
  in (True, seenA', seenB')
isSubtypeOf (Null ida) (Null idb) seenA seenB =
  let seenA' = Set.insert ida seenA
      seenB' = Set.insert idb seenB
  in (True, seenA', seenB')
isSubtypeOf a b seenA seenB = (False, seenA, seenB)
