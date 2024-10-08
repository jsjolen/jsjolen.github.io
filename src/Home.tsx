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
      I'm a software engineer with a MSc in Computer Science from KTH Royal Institute of Technology (also civilingenjör for any Swede out there).
      I have a wide array of interests including but not limited to the following.</p>
      <ul className="centered-limited">
        <li>Retro stuff, like old computers and programming systems</li>
        <li>Compilers and PL theory</li>
        <li>Formal methods</li>
        <li>Security</li>
        <li>Distributed systems</li>
        <li>Functional programming</li>
      </ul>
      <p className="centered-limited">
      You may contact me at: johan.borglin.sjolen at gmail dot com.
      I can also be found at FOSSTodon under the handle jsjolen.
      </p>
<p className="centered-limited">
    I am currently employed by Oracle as part of the Java Platform Group. I work on the Hotspot runtime, it's a lot of fun and really cool!
    Any views and opinions I write here are my own and do not represent my employer.
</p>
      <p className="centered-limited"> Thanks for reading, cheers! </p>
    </div>
  );
}
