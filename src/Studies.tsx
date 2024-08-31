import React, { useEffect } from 'react';
import { BSc } from './BSc';
import { RelSymWasm } from './RelSymWasm';

export function Studies() {
  return <div>
    <p className="centered-limited">
    This is some of the stuff that I did while still at university. I am fond of my university studies, so it's nice to reminisce.
   </p>
  < BSc />
  < RelSymWasm />
  </div>
};
