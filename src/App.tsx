import React, { useEffect } from 'react';
import {
  Route,
  Link,
  HashRouter,
  Switch
} from 'react-router-dom';

import { RelSymWasm } from './RelSymWasm';
import { Home } from './Home';
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
      </ul>
      <Switch>
       <Route exact path="/relsymwasm" component={RelSymWasm}/>
       <Route exact path="/" component={Home}/>
      </Switch>
    </HashRouter>
  );
}

export default App;
