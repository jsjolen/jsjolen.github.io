import React from 'react';
import './pure.css';

export function Home() {
  return (
    <div>
      <header>
      <h1 style={{margin:"2em"}}>Johan Sjölén</h1>
      </header>
      <h3 className="centered-limited">Welcome </h3>
      <p className="centered-limited">
      I'm a software engineer with a MSc from KTH Royal Institute of Technology (also civilingenjör for any Swede out there).
      I have a wide array of interests including but not limited to the following.</p>
      <ul className="centered-limited">
        <li>Compilers and PL theory</li>
        <li>Formal methods</li>
        <li>Security</li>
        <li>Distributed systems</li>
        <li>Functional programming</li>
      </ul>
      <p className="centered-limited">
      On this page you will find any work I've done pertaining to these topics.
      You may contact me at: johan.borglin.sjolen at gmail dot com.
      </p>
    </div>
  );
}
