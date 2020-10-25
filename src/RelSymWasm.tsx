import React from 'react'

export function RelSymWasm() {
  return (
    <div>
      <h2 className="center">Relational Symbolic Execution in WebAssembly</h2>
      <h3 className="centered-limited">Abstract</h3>
      <p className="centered-limited">
      WebAssembly is a new low-level language used as a compilation target which runs in web browsers. As more code is run on the client side of a web application the issue of security of that code become more important. Our work is based in the approach of using formal verification in order to prove that for a program one or more security properties hold. In this thesis we have explored the usage of relational symbolic execution in order to perform formal verification of security properties for WebAssembly programs. We described a formal semantics of relational symbolic execution for WebAssembly, implemented it in the Redex framework, extended the implementation for verification of constant-time security, and used the implementation to formally verify multiple sample programs including Salsa20. Our work shows that relational verification of standard security properties such as non-interference and constant-time security by using relational symbolic execution for WebAssembly is a viable approach.
      </p>
      <a className="center" href="https://kth.diva-portal.org/smash/record.jsf?pid=diva2%3A1471675&dswid=4728">Available from DiVA here</a>
      </div>
  );
}
