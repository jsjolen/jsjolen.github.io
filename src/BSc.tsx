import React from 'react'

export function BSc() {
  return (
    <div>
      <h2 className="center">Probabilistic Least-violating Control Strategy Synthesis with Safety Rules</h2>
      <h3 className="centered-limited">Abstract</h3>
      <p className="centered-limited">
We consider the problem of automatic control strategy synthesis for discrete models of robotic systems, where the goal is to travel from some region to another while obeying a given set of safety rules in an environment with uncertain properties. This is a probabilistic extension of the work by Jana Tumová et al.  that is able to handle uncertainty by modifying the least-violating strategy synthesis algorithm. The first novel contribution is a way of modelling uncertain events in a map as a Markov decision process with a specific structure, using what we call "Ghost States". We then introduce a way of constructing a Product Automaton analogous to the original work, on which a modified probabilistic version of Dijkstra's algorithm can be run to synthesize the least-violating plan. The result is a synthesis algorithm that works similarly to the original, but can handle probabilistic uncertainty. It could be used in cases where e.g. uncertain weather conditions or the behaviour of external actors can be modelled as stochastic variables.
      </p>
      <a className="center" href="https://kth.diva-portal.org/smash/record.jsf?dswid=-5756&pid=diva2%3A1215050">Available from DiVA here</a>
      </div>
  );
}
