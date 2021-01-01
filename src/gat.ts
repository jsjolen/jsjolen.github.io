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
