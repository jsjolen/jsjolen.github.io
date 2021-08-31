import React, { FunctionComponent, ReactElement } from 'react';

export function AnalysisForFree() {
  return <p className="centered-limited">
        In 2017 some smart people published a paper called "Abstracting definitional interpreters".
        In this paper they described how a simple definitional interpreter (school-book interpreters, a la the LISP1.5 one) with some specific
  adjustments made to it becomes available as an analysis target. This is done in this paper essentially by stacking monad transformers and stuff
    </p>;
}
