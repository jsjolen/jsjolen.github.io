import React from 'react';
import {
  Route,
  Link,
  BrowserRouter,
  Switch
} from "react-router-dom";

import { RelSymWasm } from './RelSymWasm';
import { Home } from './Home';

function App() {
  return (
    <BrowserRouter>
      <Switch>
       <Route exact path="/relsymwasm" component={RelSymWasm}/>
       <Route exact path="/" component={Home}/>
      </Switch>
    </BrowserRouter>
  );
}

export default App;
