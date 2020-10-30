import React, { useState, FunctionComponent, ReactNode } from 'react';
import { Grammars, IToken } from 'ebnf';
import { Graphviz } from 'graphviz-react';

import SyntaxHighlighter from 'react-syntax-highlighter';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';

// TODO:
// Add special rendering for Thread where edge is dotted in PO
// Add special rendering for CmpExpr where edges denote true/false

const TLangGrammar =
`
Program         ::= WS* VarDecl* WS* Thread* WS*
Thread          ::= WS* "thread " WS* Var WS* "{" WS* (IfStmt | AssStmt | LockStmt | UnlockStmt)* WS* "}" WS*
VarDecl         ::= IntDecl | VolatileIntDecl | LockDecl
IntDecl         ::= "int" WS* Var " = " Expr ";" WS*
VolatileIntDecl ::= "volatile int" WS* Var " = " Expr ";" WS*
LockDecl        ::= "lock" WS* Var ";" WS*
IfStmt          ::= WS* "if" WS* "(" WS* CmpExpr WS* ")" WS* "{" Body "}" WS* "else" WS* "{" WS* Body "}" WS*
Body            ::= WS* (IfStmt | AssStmt | LockStmt | UnlockStmt)* WS*
AssStmt         ::= Var " = " Expr ";" WS*
LockStmt        ::= Var ".lock()" ";" WS*
UnlockStmt      ::= Var ".unlock()" ";" WS*

Expr            ::= Var | Value | CmpExpr | AddExpr
AddExpr         ::= Expr "+" Expr
CmpExpr         ::= Expr "==" Expr | Expr ">" Expr
Var             ::= ([a-z]|[A-Z])+
Value           ::= NUMBER

NUMBER          ::= "-"? ("0" | [1-9] [0-9]*) ("." [0-9]+)? (("e" | "E") ( "-" | "+" )? ("0" | [1-9] [0-9]*))?
WS              ::= [#x20#x09#x0A#x0D]+
`;

type TLangAstType =
  'Program' |
  'Thread' |
  'VarDecl' |
  //'Stmt' |
  'IfStmt' |
  'AssStmt' |
  'LockStmt' |
  'UnlockStmt' |
  'Expr' |
  'AddExpr' |
  'CmpExpr' |
  'Var' |
  'Value';
/*
Grammar tests.
*/
const parser = new Grammars.W3C.Parser(TLangGrammar);
/*
console.log(parser.getAST("5", "Value"));
console.log(parser.getAST("x", "Var"));
console.log(parser.getAST("volatile int x = 5;", "VarDecl"));
console.log(parser.getAST(`
thread A {
    x = 5;
    y = 6;
}
`, "Thread"));
console.log(parser.getAST(`
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
`, "IfStmt"))
console.log(parser.getAST(`
volatile int x = 5;
int y = 6;
thread A {
    y = 5;
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
}
thread B {
    y = 5;
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
}
`,"Program"));
*/

/*
  Semantic analysis
  */

function semanticAnalysis(program : IToken) : IToken {
  if(program.children[0]?.type === 'Thread') {
    throw new Error('Must define at least 1 variable');
  }
  const tidx = program.children.findIndex((v) => {
    return v.type === 'Thread';
  });
  if(tidx < 0) {
    throw new Error('Must define at least 1 thread');
  }
  return program;
}

/*
  Program order.
*/

interface POVertex {
  self: IToken;
  prev: POVertex|null;
  next: Array<POVertex>;
}

interface TLangToken extends IToken {
  type: TLangAstType;
}

/*
  cpo and _cpo depends a lot on mutable binding of next.
  I think in Haskell we solve this by lazy evaluation for filling out that list.
 */

function computeProgramOrder(node: IToken): [POVertex, Array<POVertex>] {
  const [varDecls, threadDecls] = node.children.reduce<[Array<IToken>,Array<IToken>]>(([varDecls, threadDecls], token)  => {
    switch(token.type) {
      case 'VarDecl': {
	varDecls.push(token);
	return [varDecls, threadDecls];
      }
      case 'Thread': {
	threadDecls.push(token);
	return [varDecls, threadDecls];
      }
      default: {
        throw new Error('no way!');
      }
    }
  }, [[],[]]);

  const mainThread = {self:varDecls[0], prev:null, next:[]};
  let mainThreadControlPoint = varDecls.slice(1).reduce<POVertex>((prev, token) => {
    const next = _computeProgramOrder(token, prev);
    prev.next.push(next);
    return next;
  }, mainThread);
  // Attach to each threadDecl
  const threadComponents : Array<POVertex> = new Array();
  threadDecls.forEach((thread) => {
    const threadNode : POVertex = {self: thread, prev: null, next: []};
    threadNode.next.push(_computeProgramOrder(thread, threadNode));
    threadComponents.push(threadNode);

    const startThreadVertex : POVertex = {self: thread, prev: mainThreadControlPoint, next: []};
    mainThreadControlPoint.next.push(startThreadVertex);
    mainThreadControlPoint = startThreadVertex;
  });
  return [mainThread, threadComponents]; // return start
}

function _computeProgramOrder(node: IToken, prev: POVertex): POVertex {
  switch(node.type) {
    case 'Thread': {
      // const thread = {self: node, prev, next: []};
      const Stmt = {self: node.children[1], prev, next: []};
      // .slice(1) ignore name
      node.children.slice(2).reduce<POVertex>((prev, token) => {
        const next = _computeProgramOrder(token, prev);
        prev.next.push(next);
        return next;
      }, Stmt);
      return Stmt;
    }
    case 'IfStmt': {
      const cmpNode : POVertex = {self: node.children[0], prev, next: []},
            thenNode : POVertex = {self: node.children[1], prev: cmpNode, next: []},
            elseNode : POVertex = {self: node.children[2], prev: cmpNode, next: []};
      cmpNode.next.push(thenNode);
      cmpNode.next.push(elseNode);
      (thenNode.self as IToken).children.reduce<POVertex>((prev, token) => {
        const next = _computeProgramOrder(token, prev);
        prev.next.push(next);
        return next;
      }, thenNode);
      (elseNode.self as IToken).children.reduce<POVertex>((prev, token) => {
        const next = _computeProgramOrder(token, prev);
        prev.next.push(next);
        return next;
      }, elseNode);
      return cmpNode;
    }
      //case 'Stmt':
    case 'VarDecl':
    case 'AssStmt':
    case 'LockStmt':
    case 'Expr':
    case 'Var':
    case 'UnlockStmt':
      return {self: node, prev, next: []};
  }
  console.log(node);
  throw new Error('Did not match!');
}
/*
Program order tests.
*/
/*
console.log(computeProgramOrder(parser.getAST("volatile int x = 5;", "VarDecl"), {self: 'START', prev: null, next: []}));
console.log(computeProgramOrder(parser.getAST(`
thread A {
    x = 5;
    y = 6;
}
`, "Thread"), {self: 'START', prev: null, next: []}));

console.log(computeProgramOrder(parser.getAST(`
volatile int x = 5;
int y = 6;
thread A {
    y = 5;
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
}
`,"Program"), {self: 'START', prev: null, next: []}));
console.log(computeProgramOrder(parser.getAST(`
volatile int x = 5;
int y = 6;
thread A {
    y = 5;
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
}
thread B {
    y = 5;
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
}
`,"Program"), {self: 'START', prev: null, next: []}));
*/

console.log(computeProgramOrder(parser.getAST(`
volatile int x = 5;
int y = 6;
thread A {
    y = 5;
    if (x>5) {
        y = 3;
        y = 0;
    }
    else {
       y = 2;
       y = 0;
    }
}
thread B {
    y = 5;
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
}
`,"Program")));
/*
  Render program order.
*/

function programOrderToDot([mainThreadComponent, threadComponents] : [POVertex, Array<POVertex>]): string {
  let output = 'digraph G {\n';
  const [main, nextIdx] = programOrderComponentToDot(mainThreadComponent, 0);
  const threads = threadComponents.reduce<[string, number]>(
    ([out, nextIdx]: [string, number], vertex: POVertex): [string, number] => {
      const [foo, bar] = programOrderComponentToDot(vertex, nextIdx)
      return [out+foo, bar+1];
    }, ['', nextIdx+1])[0];
  return (
    output +
    main +
    threads +
    '\n}'
  );
}

function programOrderComponentToDot(start: POVertex, startingIdx: number): [string, number] {
  let output = '';
  // No loops, we can just push
  const vertices = new Array<string>(),
        edges = new Array<string>();
  const writer = (node: POVertex, nodeIdx:number) => {
    let vidx = nodeIdx;
    // Add self-vertex if applicable
    if(node.self.type === 'Thread') {
      vertices.push(`node${nodeIdx} [label="Thread ${node.self.children[0].text} start" shape=plaintext]`);
    } else if(node.self.type === 'Body') {
      // Add first stmt in body
      vertices.push(`node${nodeIdx} [label="${node.self.children[0].text}" shape=plaintext]`);
    } else {
      vertices.push(`node${nodeIdx} [label="${(node.self as IToken).text.trim()}" shape=plaintext]`);
    }
    // Add child edges, etc.
    if(node.self.type === 'CmpExpr') {
      const thenBranch = nodeIdx+1;
      edges.push(`node${vidx} -> node${thenBranch} [label="true"]`);
      nodeIdx = writer(node.next[0], thenBranch);
      const elseBranch = nodeIdx+1;
      edges.push(`node${vidx} -> node${elseBranch} [label="false"]`);
      nodeIdx = writer(node.next[1], elseBranch);
    } else if(node.self.type === 'Body') {
      // Skip first child
      for(const child of node.next.splice(1)) {
	const cid = nodeIdx+1;
	if(child.self.type === 'Thread') {
	  edges.push(`node${vidx} -> node${cid}`);
	} else {
	  edges.push(`node${vidx} -> node${cid}`);
	}
	nodeIdx = writer(child, cid);
      }
    }
    else {
      for(const child of node.next) {
	const cid = nodeIdx+1;
	if(child.self.type === 'Thread') {
	  edges.push(`node${vidx} -> node${cid}`);
	} else {
	  edges.push(`node${vidx} -> node${cid}`);
	}
	nodeIdx = writer(child, cid);
      }
    }
    return nodeIdx;
  }
  const nodeIdx = writer(start, startingIdx);
  for(const vertex of vertices) {
    output += vertex+"\n";
  }
  for(const edge of edges) {
    output += edge+"\n";
  }
  return [output, nodeIdx];
}

/**
   Test programOrderToDot
 **/
console.log(programOrderToDot(computeProgramOrder(parser.getAST(`
volatile int x = 5;
int y = 6;
thread A {
    y = 5;
    if (x>5) {
        y = 3;
        y = 0;
}
    else {
       y = 2;
y = 0;
    }
}
thread B {
    y = 5;
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
}
`,"Program"))));

console.log(computeProgramOrder(parser.getAST(`
volatile int x = 5;
int y = 6;
thread A {
    y = 5;
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
}
thread B {
    y = 5;
    if (x>5) {
        y = 3;
    }
    else {
       y = 2;
    }
}
`,"Program")));


/*
  PO gui
*/

function Button(props: {text:string, onClick: () => void|Promise<void>}) {
  const [state, setState] = useState({active:false});
  return (
    <button className={state.active ? "pure-button pure-button-primary" : "pure-button pure-button-primary pure-button-active"}
    onClick={() => {
      setState({active: !state.active});
      props.onClick();
    }}>{props.text}</button>
  );
}

function ProgramOrderInput() {
  const [state, setState] = useState({dot:"", lastRender:"digraph G { a -> b }"});
  const handleChange = (event: any) => setState({dot: event.target.value, lastRender: state.lastRender});
  return (
    <div className="centered-limited" style={{display: "flex", flexDirection:"column"}}>
      <div style={{display: "flex", flexDirection:"row"}}>
        <textarea rows={20} cols={20} value={state.dot} onChange={handleChange} />
        <Graphviz dot={state.lastRender}/>
      </div>
      <Button text="Render program order"
              onClick={() => {
		setState({dot:state.dot, lastRender:state.dot});
	      }}/>
    </div>
  );
}

/*
  Synchronization Actions.
*/

/*
  Rendering.
*/

export function Grammar() {
  return <p className="centered-limited">{TLangGrammar}</p>;
}

function Program(props: {children: ReactNode}) {
  return <p className="centered-limited"> {props.children} </p>;
}

export function JMM() {
  return (
    <div>
    <h2 className="center">Interactive Java Memory Model</h2>
    <h3 className="centered-limited">Introduction</h3>
    <p className="centered-limited">
      The Java Memory Model (JMM) is a formalism which reduces the set of possible execution traces for Java programs.
      This is meant to allow for users and implementers to reason about concurrent programs.
      <a href="https://docs.oracle.com/javase/specs/jls/se8/html/jls-17.html">JLS chapter 17</a> defines the JMM.
      I personally did not find it very easy to grok this particular part of the spec.
      To solve this for me and for others I decided to implement a tool for exploring a limited form of the JMM
      that can run on the web. That is what this page is for :-).
      </p>
     <h3 className="centered-limited">The language</h3>
     <p className="centered-limited">
     We will be considering a much smaller language than Java for simplicity.
     We will exclude loops and recursion, and we will not allow dynamically spawning threads.
     However we will support locks and volatile and non-volatile ints as shared memory.
     The abstract grammar for the language is the following.
     </p>
     <Grammar/>
    <h3 className="centered-limited">Program order</h3>
    <ProgramOrderInput/>
    <h3 className="centered-limited">Inter-thread actions and synchronization actions</h3>
    <p className="centered-limited">
    An <i>inter-thread action</i> is something that happens that another thread can detect or influence.
    Consider the following program
    </p>
    <Program>
    {`int x = 0;
      thread A {
        x = 1;
      }
      thread B {
        if(x == 0) {
        } else {
  	// We detected a change
        }
     }`}
  </Program>
    <p className="centered-limited">
    Here thread A produces a write event and thread B produces a read event.
    On top of regular actions there are also synchronization actions.
    What follows is a list of the actions we will consider, then we will consider synchronization order.
    </p>
    
    <h3 className="centered-limited">Synchronization order</h3>
    <h3 className="centered-limited">Happens Before order</h3>
    TODO
     </div>
  );
}
