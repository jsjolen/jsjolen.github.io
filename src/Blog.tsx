import React, { FunctionComponent, ReactElement } from 'react';
import { gatPost } from './blogposts/gat';
import { StructuralTyping } from './StructuralTyping';
import { AppleSignInMock } from './AppleSignInMock';
import { AnalysisForFree } from './AnalysisForFree';
import { TranspilerHotTake } from './TranspilerHotTake';
import { PraiseOfGo } from './PraiseOfGo';

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
    Unison provides a solution for these issues.
    Unison is a statically typed, eagerly evaluated, language with algebraic effect handlers.
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
/*
pushPost({
    title: "When will we collect a set of portable of analysises?",
    date: '2021-04-16',
    id: 'analysis-for-free',
    content: AnalysisForFree(),
});

pushPost({
  title:'People have realised that the word transpiler is useless',
  date:'2021-01-27',
  id:'transpiler-not-good',
  content: TranspilerHotTake()
});
*/

pushPost({
  title: 'In praise of Go',
  date:'2021-08-31',
  id:'praise-go',
  content: PraiseOfGo()
});

pushPost({
  title:'Apple Sign-in Mocking',
  date:'2021-01-24',
  id:'apple-get-real',
  content: AppleSignInMock()
});

pushPost({
  title:'Structural subtyping',
  date:'2021-01-01',
  id:'struc-sub-type',
  content: StructuralTyping()
});


pushPost(gatPost);

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
