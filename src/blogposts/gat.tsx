import SyntaxHighlighter from 'react-syntax-highlighter';
import { docco } from 'react-syntax-highlighter/dist/esm/styles/hljs';
import React, { FunctionComponent, ReactElement } from 'react';

export const whileReg = `
pub trait While {

    fn skip() -> Self;
    fn branch(test: Self, then_b: Self, else_b: Self) -> Self;
    fn while_loop(test: Self, body: Self) -> Self;
    fn add(x: Self, y: Self) -> Self;
    fn gt(x: Self, y: Self) -> Self;
    fn num(x: i64) -> Self;
    fn bool(x: bool) -> Self;
    fn seq(x: Self, y: Self) -> Self;
    fn var(x: string) -> Self;
    fn ass(x: Self, y: Self) -> Self;
}
`;

export const whileGat = `
pub trait While {
    type IntegerDomain;
    type BooleanDomain;
    type VariableDomain;
    type UnitDomain;

    type Wrapped<A>;

    fn skip() -> Self::Wrapped<Self::UnitDomain>;
    fn branch(
        test: Self::Wrapped<Self::BooleanDomain>,
        then_b: Self::Wrapped<Self::UnitDomain>,
        else_b: Self::Wrapped<Self::UnitDomain>,
    ) -> Self::Wrapped<Self::UnitDomain>;
    fn while_loop(
        test: Self::Wrapped<Self::BooleanDomain>,
        body: Self::Wrapped<Self::UnitDomain>,
    ) -> Self::Wrapped<Self::UnitDomain>;
    fn seq(
        x: Self::Wrapped<Self::UnitDomain>,
        y: Self::Wrapped<Self::UnitDomain>,
    ) -> Self::Wrapped<Self::UnitDomain>;
    fn add(
        x: Self::Wrapped<Self::IntegerDomain>,
        y: Self::Wrapped<Self::IntegerDomain>,
    ) -> Self::Wrapped<Self::IntegerDomain>;
    fn gt(
        x: Self::Wrapped<Self::IntegerDomain>,
        y: Self::Wrapped<Self::IntegerDomain>,
    ) -> Self::Wrapped<Self::BooleanDomain>;
    fn num(x: i64) -> Self::Wrapped<Self::IntegerDomain>;
    fn bool(x: bool) -> Self::Wrapped<Self::BooleanDomain>;
    fn var(x: Self::VariableDomain) -> Self::Wrapped<Self::IntegerDomain>;
    fn ass(x: String, y: Self::Wrapped<Self::IntegerDomain>) -> Self::Wrapped<Self::UnitDomain>;
}
`;

export const evalGat = `
struct Eval<X>(
    // Given a state a transition produces a value of type X and a new state
    Box<dyn Fn(HashMap<String, i64>) -> (X, HashMap<String, i64>)>,
);
`;

export const implGat1 = `
impl<A> While for Eval<A> {
    type IntegerDomain = i64;
    type BooleanDomain = bool;
    type VariableDomain = String;
    type UnitDomain = ();

    type Unwrapped = A;
    type Wrapped<B> = Eval<B>;

`;

export const implGat2 = `
    fn num(x: i64) -> Eval<i64> {
        return Eval(Box::new(move |s| (x, s)));
    }

    fn add(a: Eval<i64>, b: Eval<i64>) -> Eval<i64> {
        return Eval(Box::new(move |s| {
            let x = a.0(s.clone());
            let y = b.0(s.clone());
            return (x.0 + y.0, s.clone());
        }));
    }
`;
export const implGat4 = `
    fn var(x: String) -> Eval<i64> {
        return Eval(Box::new(move |s| match s.get(&x) {
            Some(v) => return (v.clone(), s),
            None => panic!("Ouch, runtime error"),
        }));
    }

    fn ass(x: String, y: Eval<i64>) -> Eval<()> {
        return Eval(Box::new(move |s| {
            let v = y.0(s.clone());
            return ((), s.update(x.clone(), v.0));
        }));
    }
`;
export const implGat3 = `
    fn branch(test: Eval<bool>, then_b: Eval<()>, else_b: Eval<()>) -> Eval<()> {
        return Eval(Box::new(move |s| {
            let x = test.0(s);
            if x.0 == true {
                return then_b.0(x.1);
            } else {
                return else_b.0(x.1);
            }
        }));
    }
}
`;

export const implGat = `
}
`;

export const mainGat = `
fn program<W, A>() -> W
where
    W: While<Wrapped<A> = W<A>>,
{
    return W::branch(
        W::gt(W::num(5), W::num(0)),
        W::ass("result".to_string(), W::add(W::num(45), W::num(55))),
        W::while_loop(W::bool(true), W::skip()),
    );
}

fn main() {
    let gs: HashMap<String, i64> = HashMap::new();
    let y = program::<Eval<()>, ()>().0(gs);
    println!("{:?}", y.0);
    println!("Hello, world!");
}
`;

export const initialStyle = `
pub enum While {
    Branch(Box<While>, Box<While>, Box<While>),
    Loop(Box<While>, Box<While>),
    Add(Box<While>, Box<While>),
    Gt(Box<While>, Box<While>),
    Num(i64),
    Bool(bool),
    Seq(Box<While>, Box<While>),
  Var(V),
    Ass(String, I),
  Skip()
}
`;

export const pp = `
struct PP<'a>(Vector<&'a str>);

impl<'a> While for PP<'a> {
    type IntegerDomain = &'a str;
    type UnitDomain = &'a str;
    type VariableDomain = &'a str;

    fn skip() -> PP<'a> {
	return PP::<'a>(vector!["skip"]);
    }
    fn branch(test: PP<'a>, thenb: PP<'a>, elseb: PP<'a>) -> PP<'a> {
	let mut v = vector!["if "];
	v.append(test.0);
	v.append(vector![" then "]); v.append(thenb.0);
	v.append(vector![" else "]); v.append(elseb.0);
	return PP::<'a>(v);
    }

    fn while_loop(test: PP<'a>, body: PP<'a>) -> PP<'a> {
	let mut v = vector!["while "];
	v.append(test.0);
	v.append(vector![" do "]);
	v.append(body.0);
	return PP::<'a>(v);
    }
}
`

export const gatPost = {
  title: "Here's to tagless-final encodings in Rust with GATs.",
  date:"2020-12-20",
  id: "tagless-final-gats",
  content: [
    <p className="centered-limited">
    Tagless-final encodings allow for embedding typed domain specific languages.
      Tagless-final encodings are pretty great with regards to extensibility.
      One term can be interpreted in multiple ways and more terms can be added to the DSL without invalidating older interpreters.
      These encodings are popular in at least the Haskell and Scala communities and the best source I know for learning about them in detail is <a href="http://okmij.org/ftp/tagless-final/index.html">Oleg's website</a>.
</p>,
<p className="centered-limited">
      Standard Rust already carries us half-way to tagless-final encodings, let us develop an interpreter for a toy imperative language in a tagless-final style and then see how it changes when we use GATs.
      </p>,
      <h4 className="centered-limited">In short: Initial style</h4>,
      <p className="centered-limited">
Writing an interpreter in an initial style is very simple. The AST of the language is described as an enum and some function taking such a structure as input interprets it to some output.
If you're a beginner it might be a fun exercise to try out! Here is what the AST looks like encoded as an enum:
</p>,
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{initialStyle}</SyntaxHighlighter>,
      <p className="centered-limited">
      Note that we could have separated this enum into two, expressions and statements, giving us further static guarantees regarding the structure of our program.
      </p>,
      <h4 className="centered-limited">In walks the GAT.</h4>,
<figure>
<img style={{display: "block"}} className="centered-limited" src=""/>
<figcaption className="centered-limited">These guys knew what a GAT was back in '87.</figcaption>
</figure>,
      <p className="centered-limited">
      Instead of expressing our programs as an enum we will model them as a trait.
      Any implementation of such a trait is an interpretation of it.
      We will implement the concrete execution interpretation of this trait, other interpretations may be a pretty-printer or a type-checker.
      Below is the definition of the trait for the While language.
<figure>
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{whileGat}</SyntaxHighlighter>
<figcaption className="centered-limited">While as a trait with GATs in a final style.</figcaption>
</figure>
      We utilise the definition to constrain valid program configurations such that they are well-typed in our meta-language (Rust).
      Specifically, we now have an implicit separation between statements and expressions depending on their return type (unit meaning statement).
      We can see how this is enabled specifically through GATs through the Self::Wrapped type.
      Without GATs the equivalent would lose all of this type information and would look as such:
<figure>
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{whileReg}</SyntaxHighlighter>
<figcaption className="centered-limited">While as a trait without GATs.</figcaption>
</figure>
      Indeed, this definition is almost word for word the same as the enum we first presented.
      Alright, so we have described our eDSL, now how do we interpret it? As with any other trait, we must implement it.
      The code is quite long, so I will only show some of the more interesting rules.
      </p>,
      <p className="centered-limited">
      First we define the datatype for which the trait will be implemented.
      We will take the interpretation of a function taking a state as input and producing a value and new state.
      The state is seen as a mapping from variable names (strings) to the 64-bit integers.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{evalGat}</SyntaxHighlighter>
      Let's start our implementation of the trait, specifying each associated type with their concrete equivalents.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{implGat1}</SyntaxHighlighter>
      Alright, ceremony over! Now for the implementation of our first reduction rules.
      The num and add function does not touch the state, and instead only produces a new numerical value.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{implGat2}</SyntaxHighlighter>
      var and ass both require access to the store -- one for referencing it and one for producing assignments within it.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{implGat4}</SyntaxHighlighter>
      Branching really is why we implement our interpretation as transition functions as this allows us to only evaluate one branch at a time.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{implGat3}</SyntaxHighlighter>
      And here's how you use it. Unfortunately that's where our road ends, as this doesn't compile in Rust nightly yet :-(.
      <SyntaxHighlighter className="centered-limited" language="rust" style={docco}>{mainGat}</SyntaxHighlighter>
      </p>,
      <h4 className="centered-limited">So what?</h4>,
      <p className="centered-limited">
      Well, tagless-final style eDSLs can be quite useful.
      For example there could be a Rust eDSL for constraint programming, allowing for an interface similar to MiniZinc while compiling down to different solvers depending on what you need.
      Of course, HKT also allows for monads, which could compete in this space by means of Monadic Constraint Programming (Schrijvers, Stuckey, Wadler).
For me it'd be interesting to see if these can be combined with procedural macros in some cool manner of compile-time programming.
      I have no idea if this is possible, this is actually my first Rust program :-).
    Oleg's website has a  lot of examples of when tagless-final is useful and it's a very Google-able term, check it out!
      </p>,
    <p className="centered-limited">
      There are <a href="https://degoes.net/articles/tagless-horror">reasonable objections</a> to how tagless-final is used in the Scala community, especially with regards to effect management.
      I can't predict the future, and I'm not some sort of Rust influencer, so I'm not going to pretend like tagless-final is an obvious hit for the Rust community.
      But hey, at least it made for a neat blog post.
      </p>,
  ]
};
