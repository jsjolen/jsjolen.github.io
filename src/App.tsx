import React, { useEffect } from 'react';
import {
  Route,
  Link,
  HashRouter,
  Routes
} from 'react-router-dom';

import { RelSymWasm } from './RelSymWasm';
import { BSc } from './BSc';
import { Home } from './Home';
import { JMM } from './JMM';
import { Blog } from './Blog';
import './pure.css';

function App() {
  useEffect(() => {
    document.title = 'Johan Sjölén\'s web zone of cool';
  });
  return (
    <HashRouter>
      <ul className="pure-menu-horizontal">
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/">Home</Link></li>
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/blog/all">Blog</Link></li>
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/relsymwasm">MSc thesis</Link></li>
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/bsc">BSc thesis</Link></li>
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/jmm">JMM (WIP)</Link></li>
      </ul>
      <Routes>
       <Route path="/relsymwasm" element={<RelSymWasm />}/>
       <Route path="/blog/:id" element={< Blog />}/>
       <Route path="/bsc" element={<BSc />}/>
       <Route path="/jmm" element={<JMM />}/>
       <Route path="/" element={<Home />}/>
      </Routes>
    </HashRouter>
  );
}

export default App;
