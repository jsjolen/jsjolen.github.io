import React from 'react'
import { Graphviz } from 'graphviz-react';
import SyntaxHighlighter from 'react-syntax-highlighter';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';
const Component = () => {
  const codeString = '(num) => num + 1';
  return (
    <SyntaxHighlighter language="javascript" style={docco}>
      {codeString}
    </SyntaxHighlighter>
  );
};

const typeGraph = `
digraph G {
    int [label="int"];
    null [label="null"];
    llunion [label="", shape=circle];
    llrecord [label="", shape=box];
    innerunion [label="", shape=circle];
    innerrecord [label="", shape=box];
    outerunion [label="", shape=circle];
    outerrecord [label="", shape=box];

    LinkedList[shape=octagon]
    LLInner[shape=octagon]
    LLOuter[shape=octagon]

    llunion -> null;
    llunion -> llrecord;
    llrecord -> llunion;
    llrecord -> int;

    innerunion -> null;
    innerunion -> innerrecord;
    innerrecord -> int;
    innerrecord -> outerunion;

    outerunion -> null;
    outerunion -> outerrecord;
    outerrecord -> int;
    outerrecord -> innerunion;

    LLOuter -> outerunion [style=dashed];
    LLInner -> innerunion [style=dashed];
    LinkedList -> llunion [style=dashed];
}
`;

const hsCode = `
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
import qualified Data.Set as Set
import Debug.Trace

data StructuralType =
  Union [StructuralType] Integer
  | Record [StructuralType] Integer
  | Int Integer
  | Null Integer
deriving instance Eq StructuralType
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
-- Remainder omitted for brevity

tinsert :: StructuralType -> Set.Set Integer -> Set.Set Integer
tinsert (Record _ id) set = Set.insert id set
-- Remainder omitted for brevity

-- Requires identical ordering of elements
isSubtypeOf
  :: StructuralType ->
     StructuralType ->
     Set.Set Integer ->
     Set.Set Integer ->
     (Bool, Set.Set Integer, Set.Set Integer)
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
`;

function StructuralTyping() {
  return (
    <div>
      <h3 className="centered-limited">Introduction</h3>
      <p className="centered-limited">
      Consider structural typing. Specifically, how does the type checker produce a proof that some type <i>a</i> is a subtype of some other type <i>b</i>?
      In a nominal type system this is as easy as checking if the definition of <i>a</i> mentions that it extends <i>b</i>.
      In a structural one it becomes more complex, as we must prove that <i>a</i> has the same structure as <i>b</i>.
      This is slightly tricky when mutually recursive structures are considered.
      This is a fun exercise! Below is my solution in Haskell.
      </p>

      <h3 className="centered-limited">The language</h3>
      <p className="centered-limited">
      I won't give a formal grammar, but our type language supports the following:
      <ul>
      <li> Unions: <i>t</i> := a|b  </li>
      <li> Records: <i>t</i> := a x b </li>
      <li> Primitive types: <i>int</i>, <i>null</i> </li>
      </ul>
      A linked list can be defined as: <i>LinkedList</i> := <i>null</i> | <i>int</i> x <i>LinkedList</i>.
      </p>

      <h3 className="centered-limited">The solution</h3>
      <p className="centered-limited">
      As most things in life all that is required is the correct perspective.
      We will consider types as references to vertices in a sort of directed graph or state machine.
      The vertices are either boxes, circles, or octagons with text in them. Circles represent unions (or primitive types), boxes records. The octagons and dashed lines are named references.
      To check if <i>a</i> {"≤"} <i>b</i> is equivalent to checking if a particular trace exists in both <i>a</i> and <i>b</i>.
      Consider the following types:
      <ul className="empty">
      <li> <i>LinkedList</i> := <i>null</i> | <i>int</i> x <i>LinkedList</i> </li>
      <li> <i>LLInner</i> := <i>null</i> | <i>int</i> x <i>LLOuter</i>  </li>
      <li> <i>LLOuter</i> := <i>null</i> | <i>int</i> x <i>LLInner</i>  </li>
      </ul>
      Is <i>LinkedList</i> ≤ <i>LLInner</i>?
      </p>
      <p className="centered-limited">
      To answer this question we must first compute the type graph of this instance.
      </p>
      <div className="centered-limited" style={{display: "flex", flexDirection:"column"}}>
      <Graphviz dot={typeGraph}/>
      </div>
      <p className="centered-limited">
      We defined this graph in Haskell utilising algebraic data types with ids for labelling unique vertices.
      The idea is pretty simple. Essentially for <i>a</i> {"≤"} <i>b</i> then we must be able to take lock-step identical steps in each data structure until we are in a state in both <i>a</i> and <i>b</i> where we've been before.
      Since <i>b</i> must be a superset of <i>a</i> we let <i>a</i> decide which next step to take.
      </p>
      <SyntaxHighlighter className="centered-limited" language="haskell" style={docco}>
       {hsCode}
      </SyntaxHighlighter>
      <h3 className="centered-limited">Subtyping of functions</h3>
      <p className="centered-limited">
      Let us expand this subtyping relation to functions.
      We will consider single-argument functions, in other words our types are expanded thusly:
      <ul>
      <li> (<i>a</i> → <i>b</i>) </li>
      </ul>
      And (<i>a</i> → <i>b</i>) {"≤"} (<i>a{"'"}</i> → <i>b{"'"}</i>) iff <i>a{"'"}</i> {"≤"} <i>a</i> and <i>b</i> {"≤"} <i>b{"'"}</i>.
      In other words a function f which is a subtype of g can accept anything that g can accept and possibly more but will return at most what g may return.
      </p>
</div>
  );
}

export const structuralTypingPost = {
  title:'Structural subtyping',
  date:'2021-01-01',
  id:'struc-sub-type',
  content: StructuralTyping()
};
