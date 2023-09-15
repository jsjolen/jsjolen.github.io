import React, { useEffect } from 'react';
import {
  Route,
  Link,
  HashRouter,
  Routes
} from 'react-router-dom';

import { Home } from './Home';
import { Studies } from './Studies';
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
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/studies">My uni stuff</Link></li>
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/jmm">JMM (will never finish)</Link></li>
      </ul>
      <Routes>
       <Route path="/blog/:id" element={< Blog />}/>
       <Route path="/studies" element={<Studies />}/>
       <Route path="/jmm" element={<JMM />}/>
       <Route path="/" element={<Home />}/>
      </Routes>
    </HashRouter>
  );
}

export default App;
