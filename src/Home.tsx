import React from 'react';

export function Home() {
  return (
    <div>
      <header>
        <h1>Johan Sjölén - johan.borglin.sjolen@gmail.com</h1>
      </header>
      <p><a href="/relsymwasm">MSc thesis: Relational symbolic execution in WebAssembly</a></p>
      <p><a href={process.env.PUBLIC_URL+"/cv.pdf"}>CV (PDF)</a></p>
    </div>
  );
}
