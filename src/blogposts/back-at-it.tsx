import SyntaxHighlighter from 'react-syntax-highlighter';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';
import React, { FunctionComponent, ReactElement } from 'react';

export const backAtItPost = {
  title: "Back at it",
  date:"2023-09-15",
  id:"back-at-it",
  content: [
    <p className="centered-limited">
    I resurrected this blog. It took some work, for example create-react-app is no longer developed. I don't see why I should switch to anything else,
however. I also had to fix my usage of react-router-dom, as it had introduced some breaking API changes. NPM screamed a lot about security issues, but
as I'm not hosting the site myself I don' see why I should have to care. Over all, not too difficult to migrate forward after a few years of
  absolutely no progress!
</p>
    ]
};
