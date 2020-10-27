import React, { useState, FunctionComponent, ReactNode } from 'react';
import { Grammars, IToken } from 'ebnf';
import { Graphviz } from 'graphviz-react';

import SyntaxHighlighter from 'react-syntax-highlighter';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';

const TLangGrammar =
`
Program         ::= WS* VarDecl* WS* Thread* WS*
Thread          ::= WS* "thread " WS* Var WS* "{" WS* Stmt* WS* "}" WS*
VarDecl         ::= IntDecl | VolatileIntDecl | LockDecl
IntDecl         ::= "int" WS* Var " = " Expr ";" WS*
VolatileIntDecl ::= "volatile int" WS* Var " = " Expr ";" WS*
LockDecl        ::= "lock" WS* Var ";" WS*
Stmt            ::= (IfStmt | AssStmt | LockStmt | UnlockStmt)
IfStmt          ::= WS* "if" WS* "(" WS* CmpExpr WS* ")" WS* "{" WS* Stmt* WS* "}" WS* "else" WS* "{" WS* Stmt* WS* "}" WS*
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
  'Stmt' |
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

/*
  Program order.
*/

interface PONode {
  self: IToken|string;
  prev: PONode|null;
  next: Array<PONode>;
}

interface TLangToken extends IToken {
  type: TLangAstType;
}

function computeProgramOrder(node: IToken, prev: PONode): PONode {
  switch(node.type as TLangAstType) {
    case 'Program': {
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
	}
	throw new Error('unreachable');
      }, [[],[]]);
      const lastVar = varDecls.reduce<PONode>((prev, token) => {
        const next = computeProgramOrder(token, prev);
        prev.next.push(next);
        return next;
      }, prev);
      // Attach to each threadDecl
      threadDecls.forEach((thread) => {
	lastVar.next.push(computeProgramOrder(thread, lastVar));
      });
      return prev; // return start
    }
    case 'Thread': {
      // .slice(1) ignore name
      return node.children.slice(1).reduce<PONode>((prev, token) => {
        const next = computeProgramOrder(token, prev);
        prev.next.push(next);
        return next;
      }, {self: node, prev, next: []});
    }
    case 'IfStmt': {
      const cmpNode : PONode = {self: node.children[0], prev, next: []},
            thenNode : PONode = {self: node.children[1], prev: cmpNode, next: []},
            elseNode : PONode = {self: node.children[2], prev: cmpNode, next: []};

      cmpNode.next.push(thenNode);
      cmpNode.next.push(elseNode);

      node.children[1].children.reduce<PONode>((prev, token) => {
        const next = computeProgramOrder(token, prev);
        prev.next.push(next);
        return next;
      }, thenNode);
      node.children[2].children.reduce<PONode>((prev, token) => {
        const next = computeProgramOrder(token, prev);
        prev.next.push(next);
        return next;
      }, elseNode);
      return cmpNode;
    }
    case 'VarDecl':
    case 'AssStmt':
    case 'LockStmt':
    case 'UnlockStmt':
    case 'Stmt':
      return {self: node, prev, next: []};
  }
  throw new Error('Did not match!');
}
/*
Program order tests.
*/
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


/*
  Render program order.
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

function renderProgramOrder(start: PONode) {
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

/**

export const Form: FunctionComponent<IProps> = ({ onFinish, initState }) => {
  const formInputsStateRef = useRef({})

  const handleFinish = () => {
    const params = formInputsStateRef.current
    console.log(params)
    onFinish(params)
  }

  return (
    <div>
      <Inputs initState={initState} stateRef={formInputsStateRef}  />
      <S.Button onClick={handleFinish}>
        Finish
      </S.Button>
    </div>
  )
}

**/

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
