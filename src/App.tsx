import React, { useEffect } from 'react';
import {
  Route,
  Link,
  HashRouter,
  Switch
} from 'react-router-dom';

import { RelSymWasm } from './RelSymWasm';
import { BSc } from './BSc';
import { Home } from './Home';
import { JMM } from './JMM';
import { StructuralTyping } from './StructuralTyping';
import './pure.css';

function App() {
  useEffect(() => {
    document.title = 'A page.';
  });
  return (
    <HashRouter>
      <ul className="pure-menu-horizontal">
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/">Home</Link></li>
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/relsymwasm">MSc thesis</Link></li>
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/bsc">BSc thesis</Link></li>
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/jmm">JMM (WIP)</Link></li>
        <li className="pure-menu-item"><Link className="pure-menu-link" to="/structuraltyping">Structural typing</Link></li>
      </ul>
      <Switch>
       <Route exact path="/relsymwasm" component={RelSymWasm}/>
       <Route exact path="/bsc" component={BSc}/>
       <Route exact path="/jmm" component={JMM}/>
       <Route exact path="/structuraltyping" component={StructuralTyping}/>
       <Route exact path="/" component={Home}/>
      </Switch>
    </HashRouter>
  );
}

export default App;
