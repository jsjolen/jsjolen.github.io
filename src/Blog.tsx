import React, { FunctionComponent, ReactElement } from 'react';
import SyntaxHighlighter from 'react-syntax-highlighter';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';
import { whileGat, whileReg, pp, initialStyle, mainGat, implGat1, implGat2, implGat3, implGat4, evalGat } from './gat';
import { StructuralTyping } from './StructuralTyping';

import BlogPostImage from './blogpost.png';

import {
  useParams,
  Link
} from "react-router-dom";

type BlogPost = (props: {title:string, date:string, content: any, id:string}) => ReactElement<any,any>;
const Post: BlogPost = (props: {title:string, date:string, content: any, id:string}): ReactElement<any,any> => {
  return (
    <div>
      <h2 className="centered-limited"><Link to={`${props.id}`}>{props.title}</Link></h2>
      <h3 className="centered-limited">{props.date}</h3>
    {props.content}
    </div>
  );
};

const Footnote = (props:{id:string, num:number, children: ReactElement<any,any>}) => {
  return <p className="centered-limited" id={props.id}>[{props.num}] {props.children}</p>;
};
const FootnoteRef = (props:{id:string, num:number}) => {
  return <a>[{props.num}]</a>
};

const posts: {title:string, date:string, content: any, id:string}[] = [];
const idToPost: Map<string, any> = new Map();

const pushPost = (post:{title:string, date:string, content: any, id:string}) => {
  posts.push(post);
  idToPost.set(post.id, post);
};
pushPost({
  title:'Structural subtyping',
  date:'2021-01-01',
  id:'struc-sub-type',
  content: StructuralTyping()
});

/*
pushPost({
  title:"Unison - The statically typed answer to Common Lisp?]",
  date:"2020-12-17",
  id:"unison-cl",
  content: [
    <h4 className="centered-limited"> Introduction </h4>,
    <p className = "centered-limited">
  Common Lisp's claim to fame is probably its excellent support for interactive development.
With an interactive debugger and the full system (including compiler) available at runtime CL enables programmers to
interactively compile, test, and run their porgrams without ever restarting the CL process.
At least that's the story which CL:ers would like to tell themselves.
    In reality, entering an undesirable state is easy and recovering from it may be difficult, with the most commonly opted for solution is to kill the process and restart from a clean slate.
    Because of its dynamically typed nature, manual refactorings cannot be checked for correctness, and allowing for functions to be re-defined at runtime makes static analysis difficult.
    Despite all of these flaws CL remains the favourite language for some, precisely because of the affordances that these flaws gives the user.
    </p>,
    <p className = "centered-limited">
    Unison provides a solution (note: a solution is not a silver bullet) for these issues.
    Unison is a statically typed, eagerly evaluated, language with algebraic effect handlers (conditions, but better).
    What makes Unison unique is the key technical idea as listed on their website: "Unison definitions are identified by content".
    Taking this idea and running with it has proven to construct a language which I believe that Lispers would find appealing.
    The remainder of this post will mostly be regurgitation - any facts about Unison can just as well be learnt from their website - but it will be regurgitation from the perspective of a CL:er.
    I will compare and contrast the feature set of Unison with that of Common Lisp, and what language has to learn from each other.
    </p>,
    <h4 className="centered-limited"> Effects - conditions but better </h4>,
    <h4 className="centered-limited"> Safe refactoring </h4>,
    <h4 className="centered-limited"> Codebase management </h4>
    ]
});

pushPost({
  title:"Galois Connections",
  date:"2020-12-17",
  id:"galois-connections",
  content:
  <p className = "centered-limited">
  </p>
});
*/

/*
pushPost({
  title:"Compilation is easy and don't let no one tell you otherwise.",
  id: "stack-compiler-easy",
  date:"2020-12-30",
  content: [
    <p className="centered-limited">
    It seems like people think that compilation is hard, and that could not be further from the truth.
      Okay, compilation can involve difficult tasks such as register allocation (NP-complete), semantic and static analysis, lexing, parsing, closure-lifting, re-writing into CPS, re-writing into SSA, and more.
      Heck, with the advent of the language server protocol a compiler engineer will probably have to implement the compiler as a constantly running background process which keeps around parsed files
      such that as little as possible has to be re-computed in order to serve LSP requests.
      But really, assuming that you have a parser then compilation of a bog standard, statically typed, imperative language into a stack-based VM is very easy.
    </p>
  ]
});
*/

pushPost({
  title: "Here's to tagless-final encodings in Rust with GATs.",
  date:"2020-12-20",
  id: "tagless-final-gats",
  content: [
    <p className="centered-limited">
    Tagless-final encodings allow for embedding typed domain specific languages.
      Tagless-final encodings are pretty great with regards to extensibility.
      One term can be interpreted in multiple ways and more terms can be added to the DSL without invalidating older interpreters.
      These encodings are popular in at least the Haskell and Scala communities and the best source I know for learning about them in detail is <a href="http://okmij.org/ftp/tagless-final/index.html">Oleg's website</a>.
</p>,
<p className="centered-limited">
      Standard Rust already carries us half-way to tagless-final encodings, let us develop an interpreter for a toy imperative language in a tagless-final style and then see how it changes when we use GATs.
      </p>,
      <h4 className="centered-limited">In short: Initial style</h4>,
      <p className="centered-limited">
Writing an interpreter in an initial style is very simple. The AST of the language is described as an enum and some function taking such a structure as input interprets it to some output.
If you're a beginner it might be a fun exercise to try out! Here is what the AST looks like encoded as an enum:
</p>,
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{initialStyle}</SyntaxHighlighter>,
      <p className="centered-limited">
      Note that we could have separated this enum into two, expressions and statements, giving us further static guarantees regarding the structure of our program.
      </p>,
      <h4 className="centered-limited">In walks the GAT.</h4>,
<figure>
<img style={{display: "block"}} className="centered-limited" src="https://www.gannett-cdn.com/-mm-/3ac9d0f81e497476b6e193a9b47176e127cdc309/c=0-33-2214-1284/local/-/media/2015/10/08/USATODAY/USATODAY/635798662187573581-NWA.jpg"/>
<figcaption className="centered-limited">These guys knew what a GAT was back in '87.</figcaption>
</figure>,
      <p className="centered-limited">
      Instead of expressing our programs as an enum we will model them as a trait.
      Any implementation of such a trait is an interpretation of it.
      We will implement the concrete execution interpretation of this trait, other interpretations may be a pretty-printer or a type-checker.
      Below is the definition of the trait for the While language.
<figure>
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{whileGat}</SyntaxHighlighter>
<figcaption className="centered-limited">While as a trait with GATs in a final style.</figcaption>
</figure>
      We utilise the definition to constrain valid program configurations such that they are well-typed in our meta-language (Rust).
      Specifically, we now have an implicit separation between statements and expressions depending on their return type (unit meaning statement).
      We can see how this is enabled specifically through GATs through the Self::Wrapped type.
      Without GATs the equivalent would lose all of this type information and would look as such:
<figure>
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{whileReg}</SyntaxHighlighter>
<figcaption className="centered-limited">While as a trait without GATs.</figcaption>
</figure>
      Indeed, this definition is almost word for word the same as the enum we first presented.
      Alright, so we have described our eDSL, now how do we interpret it? As with any other trait, we must implement it.
      The code is quite long, so I will only show some of the more interesting rules.
      </p>,
      <p className="centered-limited">
      First we define the datatype for which the trait will be implemented.
      We will take the interpretation of a function taking a state as input and producing a value and new state.
      The state is seen as a mapping from variable names (strings) to the 64-bit integers.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{evalGat}</SyntaxHighlighter>
      Let's start our implementation of the trait, specifying each associated type with their concrete equivalents.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{implGat1}</SyntaxHighlighter>
      Alright, ceremony over! Now for the implementation of our first reduction rules.
      The num and add function does not touch the state, and instead only produces a new numerical value.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{implGat2}</SyntaxHighlighter>
      var and ass both require access to the store -- one for referencing it and one for producing assignments within it.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{implGat4}</SyntaxHighlighter>
      Branching really is why we implement our interpretation as transition functions as this allows us to only evaluate one branch at a 
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{implGat3}</SyntaxHighlighter>
      And here's how you use it. Unfortunately that's where our road ends, as this doesn't compile in Rust nightly yet.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{mainGat}</SyntaxHighlighter>
      </p>,
      <h4 className="centered-limited">So what?</h4>,
      <p className="centered-limited">
      Well, tagless-final style eDSLs can be quite useful.
      For example there could be a Rust eDSL for constraint programming, allowing for an interface similar to MiniZinc while compiling down to different solvers depending on what you need.
      Of course, HKT also allows for monads, which could compete in this space by means of Monadic Constraint Programming (Schrijvers, Stuckey, Wadler).
      For me, I am interested in allowing for eDSLs which themselves have custom type systems.
      Such type systems can be expressed by means of abstract interpretation and could be run as part of the tests.
      Oleg's website has a  lot of examples of when tagless-final is useful.
      </p>,
    <p className="centered-limited">
      There are <a href="https://degoes.net/articles/tagless-horror">reasonable objections</a> to how tagless-final is used in the Scala community, especially with regards to effect management.
      I can't predict the future, and I'm not some sort of Rust influencer, so I'm not going to pretend like tagless-final is an obvious hit for the Rust community.
      But hey, at least it made for a neat blog post.
      </p>,
  ]
});

pushPost({
  title: "How I run this site.",
  date:"2020-12-18",
  id:"how-i-run-this-site",
  content: [
    <p className="centered-limited">
      This is the KISS version of running a personal site.
      I got my css from pure-css and I run everything as a React app using gh-pages and react-scripts.
      Every blog post is a small program which pushes a post onto an array which is then rendered by a function (I use React Hooks).
      I treat the entire website as a program where I run whatever I want.
      There are essentially no static assets.
      I can whole-heartedly recommend this as a no-frills version of writing and hosting a personal site.
    </p>,
      <p className="centered-limited">
      Unfortunately the site takes an absurd amount of memory.
      Loading the site can take a while, according to Firefox almost 2MB of compressed JavaScript is sent over the wire.
      I assume that the majority of this is the libraries which I import.
      </p>,
    <img style={{display: "block"}} className="centered-limited" src={BlogPostImage} alt="How I wrote this blog post">
    </img>
  ]
})

export const Blog = () => {
  const { id } = useParams<{id:string}>();
  if(id === 'all') {
    return (
      <div>
	<h1 className="center">Blog</h1>
	{posts.map(Post)}
      </div>
    );
  } else {
    const post = idToPost.get(id);
    if(post) {
      return (
	<div>{Post(post)}</div>
      );
    }
    else {
      return (
	<div>
	  <h1 className="center">Blog post not found.</h1>
	  </div>
      );
    }
  }
}
