import React, { FunctionComponent, ReactElement } from 'react';

import {
  useParams,
  Link
} from "react-router-dom";

type BlogPost = (props: {title:string, date:string, content: any, id:string}) => ReactElement<any,any>;
const Post: BlogPost = (props: {title:string, date:string, content: any, id:string}): ReactElement<any,any> => {
  return (
    <div>
      <h3 className="centered-limited"><Link to={`${props.id}`}>{props.title}</Link></h3>
      <h3 className="centered-limited">{props.date}</h3>
    {props.content}
    </div>
  );
};

const posts: any[] = [];
const idToPost: Map<string, any> = new Map();

const pushPost = (post:any) => {
  posts.push(post);
  idToPost.set(post.id, post);
};

pushPost({
  title:"Unison - The statically typed answer to Common Lisp?",
  date:"2020-12-17",
  id:"unison-cl",
  content:
    <p className = "centered-limited">
  Common Lisp's claim to fame is probably its excellent support for interactive development.
With an interactive debugger and the full system (including compiler) available at runtime CL enables programmers to
interactively compile, test, and run their porgrams without ever restarting the CL process.
At least that's the story which CL:ers would like to tell themselves.
    In reality, entering an undesirable state is easy and recovering from it may be difficult, with the most commonly opted for solution is to kill the process and restart from a clean slate.
    Because of its dynamically typed nature, manual refactorings cannot be checked for correctness, and allowing for functions to be re-defined at runtime makes static analysis difficult.
    Despite all of these flaws CL remains the favourite language for some, precisely because of the affordances that these flaws gives the user.
    </p>
});

pushPost({
  title:"Galois Connections",
  date:"2020-12-17",
  id:"galois-connections",
  content:
  <p className = "centered-limited">
  </p>
});

export const Blog = () => {
  const { id } = useParams<{id:string}>();
  console.log(id);
  if(id == 'all') {
    return (
      <div>
	<h2 className="center">Blog</h2>
	{posts.reverse().map(Post)}
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
