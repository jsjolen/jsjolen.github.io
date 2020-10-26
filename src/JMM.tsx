import React from 'react';
import { Grammars, IToken } from 'ebnf';

import SyntaxHighlighter from 'react-syntax-highlighter';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';

const TLangGrammar =
`
Program     ::= WS* VarDecl* WS* Thread* WS*
Thread      ::= WS* "thread " WS* Var WS* "{" WS* Stmt* WS* "}" WS*
VarDecl     ::= ("volatile int"|"int") WS* Var " = " Expr ";" WS* | "lock" WS* Var ";" WS*

Stmt        ::= (IfStmt | AssStmt | LockStmt | UnlockStmt)
IfStmt      ::= WS* "if" WS* "(" WS* CmpExpr WS* ")" WS* "{" WS* Stmt* WS* "}" WS* "else" WS* "{" WS* Stmt* WS* "}" WS*
AssStmt     ::= Var " = " Expr ";" WS*
LockStmt    ::= Var ".lock()" ";" WS*
UnlockStmt  ::= Var ".unlock()" ";" WS*

Expr        ::= Var | Value | CmpExpr | AddExpr
AddExpr     ::= Expr "+" Expr
CmpExpr      ::= Expr "==" Expr | Expr ">" Expr
Var         ::= ([a-z]|[A-Z])+
Value       ::= NUMBER

NUMBER      ::= "-"? ("0" | [1-9] [0-9]*) ("." [0-9]+)? (("e" | "E") ( "-" | "+" )? ("0" | [1-9] [0-9]*))?
WS          ::= [#x20#x09#x0A#x0D]+
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
      return node.children.slice(1).reduce<PONode>((prev, token)  => {
        const next = computeProgramOrder(token, prev);
        prev.next.push(next);
        return next;
      }, {self: node.children[0], prev, next: []});
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

/*
  Render program order.
*/

/*
  Synchronization Actions.
*/

/*
  Rendering.
*/

export function Grammar() {
  return <p>TODO</p>;
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
    TODO
    <h3 className="centered-limited">Synchronization actions</h3>
    TODO
     <h3 className="centered-limited">Happens Before order</h3>
    TODO
     </div>
  );
}
