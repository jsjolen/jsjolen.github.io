import React, { FunctionComponent, ReactElement } from 'react';

export function TranspilerHotTake() {
  return ([
    <p className="centered-limited">
      The word transpiler used to be in constant use, especially in the JS community.
      It was<a href="https://web.archive.org/web/20150215181640/https://babeljs.io/">used to describe Babel</a>, and it still has its own Wikipedia article under them term "Source-to-source compiler" where projects such as CoffeeScript are described as "transpilers". It's not only used by engineers, but also by academics, as can be seen by searching for the term on Google Scholar.
      Anecdotally it's still used by some, for example I have seen people on Reddit using the term and one of the comments I had from my peer-reviewer for my MSc thesis was that the word translator should be swapped with transpiler.
      It has its own Wikipedia article under the name "Source-to-source compiler" and becoming actively used around 2014 according to Google trends.
      However, things have changed and the term seems to have fallen out of favor.
      Why? No idea. Am I happy about this? Yes, very much so. I believe that the term is almost completely useless and instead "compiler" or "translator" ought to be used.
      </p>,
      <h3 className="centered-limited"> What is a compiler, really?</h3>,
      <p className="centered-limited">
      A compiler is a function from one language to another. That is it, really.
      Yes, we typically see a compiler as having a front-end consisting of a lexer and parser, perhaps some middleware performing semantic analysis, and a backend producing code which is executable somehow.
      This is a pragmatic view of a compiler. No one will be happy if I sell them a C compiler which compiles into a subset of Klingon, or which only expands ternary operators into full if-then-else statements.
      But today what we mean by "executable code" varies. Rust's compiler only compiles to LLVM's intermediate representation which is not directly executable on any machine. Does Rust then only have a transpiler?
      Of course not!
      Was CFront only a preprocessor since it translates C++ into C? No!
      Is Babel a transpiler because it translates ES6 into ES5? No!
    </p>,
      <h3 className="centered-limited">But a transpiler compiles between two high-level languages</h3>,
      <p className="centered-limited">
      Would Typescript be a transpiler then? Probably not, tsc does type checking, 
      </p>,
  ]);
}
