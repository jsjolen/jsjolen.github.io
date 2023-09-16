import React, { FunctionComponent, ReactElement } from 'react';
import { gatPost } from './blogposts/gat';
import { backAtItPost } from './blogposts/back-at-it';
import { StructuralTyping } from './StructuralTyping';
import { AnalysisForFree } from './AnalysisForFree';
import BlogPostImage from './blogpost.png';

import {
  useParams,
  Link
} from "react-router-dom";

const linkStyle = {
    color: 'black',
    textDecoration: "underline"
}

type BlogPost = (props: {title:string, date:string, content: any, id:string}) => ReactElement<any,any>;
const Post: BlogPost = (props: {title:string, date:string, content: any, id:string}): ReactElement<any,any> => {
  return (
    <div>
      <h2 className="centered-limited"><Link style={linkStyle} to={`${props.id}`}>{props.title}</Link></h2>
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

pushPost(backAtItPost);

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
	    {posts
          .sort((a, b) => Date.parse(a.date) - Date.parse(b.date))
          .reverse()
          .map(Post)}
      </div>
    );
  } else {
    // Complained about type issue, but I don't see the problem.
    const post = idToPost.get(id || "");
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
