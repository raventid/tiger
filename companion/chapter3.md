# Parsing

The main point of the introductino here is to say that we will need a more powerful tool than finite automata to parse a language described by grammar.


## 3.1 Context-free grammars

For context free grammar I will prepare a little conspects that might help you to understand topic better

Earlier in the chapter we will take a look at this syntax for straight-line programs.

$$
\begin{aligned}
1.\quad & S \rightarrow S \; ; \; S \\
2.\quad & S \rightarrow \text{id} := E \\
3.\quad & S \rightarrow \text{print} \; ( \; L \; ) \\
4.\quad & E \rightarrow \text{id} \\
5.\quad & E \rightarrow \text{num} \\
6.\quad & E \rightarrow E + E \\
7.\quad & E \rightarrow ( \; S \; , \; E \; ) \\
8.\quad & L \rightarrow E \\
9.\quad & L \rightarrow L \; , \; E
\end{aligned}
$$

### Derviations

Later we will build a Derivation 3.2, based on the syntax above. 

<u>S</u> \
S ; <u>S</u> \
<u>S</u> ; id := E \
id := <u>E</u> ; id := E \
id := num ; id := <u>E</u> \
id := num ; id := E + <u>E</u> \
id := num ; id := <u>E</u> + ( S , E ) \
id := num ; id := id + ( <u>S</u> , E ) \
id := num ; id := id + ( id := <u>E</u> , E ) \
id := num ; id := id + ( id := E + E , <u>E</u> ) \
id := num ; id := id + ( id := <u>E</u> + E , id ) \
id := num ; id := id + ( id := num + <u>E</u> , id ) \
id := num ; id := id + ( id := num + num , id ) \


The book does not ask you to do this, but you can build the leftmost derivation as an exercise, just to check your understanding.

Derivation is just a one possible fully substitute instance of the syntax.

(So think of it as just randomly generated syntax tree)

### Parse trees

I hope parse tree is well explained (and simple). We just connect every element with the level above.

if you look at Derivation 3.2 above:

First comes S, we unwrap it into S ; S, then we unwrap first S into id := E, id := E this is 3 symbols, so we cannot every one of them to their parent. This is pretty much it, really nothing special or tricky.

### Ambiguous Grammars

<quote>A grammar is ambiguous if it can derive a sentence with two different parse trees.</quote>

\[
\begin{array}{lll}
E \rightarrow E + T \qquad & T \rightarrow T * F \qquad & F \rightarrow \textrm{id} \\
E \rightarrow E - T        & T \rightarrow T \mathbin{/} F & F \rightarrow \textrm{num} \\
E \rightarrow T            & T \rightarrow F               & F \rightarrow (\, E \,)
\end{array}
\]

Okay, Appel shows us this new grammar, how exactly it fixes the problem.

It might be hard to believe, but actually this grammar already introduced bindings and precedence!

You might know that for precedence we need some kind of priority table:

Multiplication, division - priority 8, summation and subtraction - priority 7. So first we do multiplication and only later summation.
This grammar does it in a different tricky way. Easier to see an example.

#### Binding power (precedence) via stratification
The grammar has three "levels" of nonterminals:

E (expressions) — handles + and − (lowest precedence)
T (terms) — handles * and / (higher precedence)
F (factors) — atoms: id, num, ( E ) (highest)
Because E → E + T can only have a T on the right of +, a * expression gets "trapped" inside the T and must sit lower in the parse tree. So 1 + 2 * 3 can only parse as 1 + (2 * 3) — the tree where + grabs 2 is simply not derivable.

#### Associativity via left recursion
The rules are left-recursive: E → E + T, not E → T + E. This forces 1 - 2 - 3 to parse as (1 - 2) - 3, i.e., left-associative. With the ambiguous grammar E → E - E, both trees were possible.

#### Example
Let's derive id + num * id (think a + 2 * b) and watch how the grammar forces the right structure.

Derivation (leftmost)
Using the underline convention for the nonterminal being expanded:

<u>E</u> \
<u>E</u> + T \
<u>T</u> + T \
<u>F</u> + T \
id + <u>T</u> \
id + <u>T</u> * F \
id + <u>F</u> * F \
id + num * <u>F</u> \
id + num * id \

Notice the key moments:

Step 1: The only way to get a `+` at the top is `E → E + T`. There is no rule that puts `*` at the E-level, so `+` is forced to be the root.
Steps 2–4: The left operand E must eventually shrink down through E → T → F → id. This "chain" is the price of stratification — single-child steps that just move down a level.
Step 5: The right operand is a T, and only T-rules can produce *. So num * id gets built entirely inside the T subtree — below the +.
Parse tree

            E
          / | \
         E  +  T
         |   / | \
         T  T  *  F
         |  |     |
         F  F     id
         |  |
         id num

The * lives strictly below the + — multiplication binds tighter, automatically.

Why the "wrong" tree is impossible
For + to bind tighter (i.e., (id + num) * id), we'd need a tree like:

            T
          / | \
         T  *  F        ← needs T ⇒ id + num

But to derive id + num from T, we'd need a T-rule producing +. The only T-rules are T → T * F | T / F | F — no + anywhere. The only escape hatch is F → ( E ), which is exactly why writing (a + 2) * b with parentheses works: the parens let you re-enter the E-level from below.

Associativity example: id - id - id
<u>E</u> \
<u>E</u> - T \
<u>E</u> - T - T \
...

The recursion E → E - T can only grow on the left, so the tree comes out as:

            E
          / | \
         E  -  T          ← rightmost "-" is the root
       / | \   |
      E  -  T  (id)
      |     |
     (id)  (id)

which is (id - id) - id — left-associative, exactly as subtraction should be. If the rule had been E → T - E (right-recursive) instead, you'd get id - (id - id), which computes the wrong answer.

To really feel confindent about this - the only way is to practice on paper with pen, and draw multiple trees or just write multiple transformations using the syntax rules.

I will show one example of how you can play with the grammar and see how grammar affects your language.
Original (left-associative *):

latex
\[
\begin{aligned}
T &\rightarrow T * F 
\end{aligned}
\]

Right-associative *:

latex
\[
\begin{aligned}
T &\rightarrow F * T 
\end{aligned}
\]

That's it — the recursive nonterminal moves from the left side of * to the right side and this way we can change multiplication associativity from left to right.

Why it works: let's derive id * id * id
With T → F * T, the recursion can only grow rightward:

<u>T</u> \
<u>F</u> * T \
id * <u>T</u> \
id * <u>F</u> * T \
id * id * <u>T</u> \
id * id * <u>F</u> \
id * id * id \

The first * we commit to is the leftmost one, so it becomes the root — everything after it nests inside the right subtree.

Side-by-side parse trees
Left-recursive T → T * F:   (id * id) * id
    
            T
          / | \
         T  *  F
       / | \   |
      T  *  F  id
      |     |
      F     id
      |
      id
  
Right-recursive T → F * T:   id * (id * id)

        T
      / | \
     F  *  T
     |    / | \
     id  F  *  T
         |     |
         id    F
               |
               id

But as you can see in the right recursive version our last multiplication happens to be in a nested part of the tree!

Last final element of understanding we need to have is to peak a bit into the future chapters. We didn't talk about execution of the code yet, but to understand why
left-recursive tree is really left recursive we need to understand how execution would happen. First we compute the leaf nodes, and then we go up.

I hope you are familiar ( for this book it is necessary ) with basic data structures and algorithms. So what we are doing here is postorder traversal of a tree. Let's imagine that our id is 2!

Snapshot 0 — parse tree, nothing evaluated:

            T
          / | \
         T  *  F
       / | \   |
      T  *  F  2
      |     |
      F     F
      |     |
      2     2

Snapshot 1 — leaves pass up through the F/T chains (F → 2, T → F just forward the value):

            T
          / | \
         T  *  2
       / | \
      2  *  2

Snapshot 2 — the deepest * fires: 2 * 2 = 4. Its whole subtree collapses:

            T
          / | \
         4  *  2

Snapshot 3 — root * fires: 4 * 2 = 8:

            8


You can draw the same series of transformations with the right associative tree - result will be the same. But result would be different if we would change the grammar for division operation (you can think why is it the case!)

## 3.2 Predictive parsing

Chapter opens with a simple parser for the Grammar 3.11:

\[
\begin{array}{l@{\qquad\qquad}l}
S \rightarrow \textrm{if } E \textrm{ then } S \textrm{ else } S
  & L \rightarrow \textrm{end} \\
S \rightarrow \textrm{begin } S\ L
  & L \rightarrow \textrm{;}\ S\ L \\
S \rightarrow \textrm{print } E
  & E \rightarrow \textrm{num} = \textrm{num}
\end{array}
\]


The book prints this in Standard ML (Appel's original edition uses ML), so if you copy it verbatim it will *not* compile with the OCaml toolchain we use here — `datatype`, `val`, `fun f() = ...` and `case ... of ... =>` are all SML spellings. Here is the same parser translated to OCaml that actually compiles and runs:

```ocaml
(* Predictive (recursive-descent) parser for Grammar 3.11. *)

type token =
  | IF | THEN | ELSE | BEGIN | END | PRINT
  | SEMI | NUM | EQ
  | EOF

exception Syntax_error

(* A stand-in token source. In a real compiler this is the lexer from
   Chapter 2; here we just replay a fixed list so the example runs. *)
let input = ref []
let get_token () =
  match !input with
  | [] -> EOF
  | t :: rest -> input := rest; t
let error () = raise Syntax_error

let tok = ref (get_token ())
let advance () = tok := get_token ()
let eat t = if !tok = t then advance () else error ()

let rec s () =
  match !tok with
  | IF    -> eat IF; e (); eat THEN; s (); eat ELSE; s ()
  | BEGIN -> eat BEGIN; s (); l ()
  | PRINT -> eat PRINT; e ()
  | _     -> error ()

and l () =
  match !tok with
  | END  -> eat END
  | SEMI -> eat SEMI; s (); l ()
  | _    -> error ()

and e () = eat NUM; eat EQ; eat NUM
```

What changed, and why the book's snippet does not compile as-is:

- `datatype token = ...` becomes `type token = ...`. I also added an explicit `EOF` token so the token stream has an end marker.
- `val tok = ref (getToken())` becomes `let tok = ref (get_token ())`; SML `fun f() = ...` becomes OCaml `let f () = ...`.
- `case !tok of P => e | ...` becomes `match !tok with P -> e | ...`. SML uses `=>` in a case arm and `->` only in function types; OCaml uses `->` for both.
- The three mutually-recursive functions are joined with `let rec s () = ... and l () = ... and e () = ...`. (I lowercased `S`, `L`, `E` because OCaml value names must start with a lowercase letter — capitalized identifiers are reserved for constructors and modules.)
- `getToken` and `error` were never defined in the book — they are left as "assume these exist". OCaml will not link an undefined name, so I supplied a tiny `get_token`/`error` and a fake `input` list to make the file self-contained.
- I added a `| _ -> error ()` arm to `s` and `l`. The book's `case` is deliberately non-exhaustive: an unexpected token *is* the syntax error. In SML that surfaces as a runtime `Match` exception; OCaml warns about the missing case at compile time, so making the "anything else" arm explicit is both cleaner and exactly the intended meaning.

You can drop a small driver on the end and run it to watch the parser accept and reject inputs:

```ocaml
let parse tokens =
  input := tokens;
  tok := get_token ();
  match (try s (); !tok = EOF with Syntax_error -> false) with
  | true  -> print_endline "accept"
  | false -> print_endline "reject"

let () =
  (* begin print 1=1 ; print 2=2 end *)
  parse [BEGIN; PRINT; NUM; EQ; NUM; SEMI; PRINT; NUM; EQ; NUM; END];
  (* if 1=1 then print 3=3 else print 4=4 *)
  parse [IF; NUM; EQ; NUM; THEN; PRINT; NUM; EQ; NUM; ELSE; PRINT; NUM; EQ; NUM];
  (* malformed: print with no expression *)
  parse [PRINT; SEMI]
```

Save it as `parse311.ml` and run `ocaml parse311.ml` — it prints `accept`, `accept`, `reject`. Cool? This is a real recursive decent parser we have here! As you can see this parser code literally repeats our grammar structure (and as you might guesse because of this it should be very easy to generate parsers automatically from a grammar definitions!)


Next Appel suggest to write a similar parser, but for this grammar (3.10):

\[
\begin{array}{l@{\qquad\qquad}l@{\qquad\qquad}l}
S \rightarrow E\ \$ & & \\[4pt]
  & T \rightarrow T \ast F         & F \rightarrow \textrm{id} \\
E \rightarrow E + T
  & T \rightarrow T \mathbin{/} F  & F \rightarrow \textrm{num} \\
E \rightarrow E - T
  & T \rightarrow F                & F \rightarrow (\ E\ ) \\
E \rightarrow T & &
\end{array}
\]


Appel writes the skeleton like this (I've de-garbled it and put it in OCaml shape). Notice the `?` in the `E` and `T` cases — those are *holes Appel cannot fill in*, and that is the whole point:

```ocaml
(* NAIVE attempt for Grammar 3.10 -- does NOT work, and cannot be completed. *)
let rec s () = e (); eat EOF
and e () = match !tok with
  | ? -> e (); eat PLUS;  t ()      (* E -> E + T *)
  | ? -> e (); eat MINUS; t ()      (* E -> E - T *)
  | ? -> t ()                       (* E -> T     *)
and t () = match !tok with
  | ? -> t (); eat TIMES; f ()      (* T -> T * F *)
  | ? -> t (); eat DIV;   f ()      (* T -> T / F *)
  | ? -> f ()                       (* T -> F     *)
and f () = match !tok with
  | ID     -> eat ID
  | NUM    -> eat NUM
  | LPAREN -> eat LPAREN; e (); eat RPAREN
  | _      -> error ()
```

<quote>There is a conflict here: the E function has no way to know which clause to
use. Consider the strings (1*2-3) +4 and (1*2-3). In the former case, the
initial call to E should use the E - • E + T production, but the latter case
should use E —> T</quote>

Appel says there's a conflict: the E function has no way to know which clause to use. Let's feel that by role-playing as a predictive (no-backtracking) parser hand-executing S → E $. Cursor ▸ marks the next unconsumed token.

▸( 1 * 2 - 3 ) + 4 $        goal: parse E, then eat $

Move 1. To parse S we call E. The cursor is on (. Three clauses are open:

| ? -> e (); eat PLUS;  t ()      (* E → E + T *)
| ? -> e (); eat MINUS; t ()      (* E → E - T *)
| ? -> t ()                       (* E → T     *)

Which one? All we can see is the token (. We know an expression follows, but not its shape — is there a top-level + waiting at the far end, or not? We can't tell from here, so we take a guess and commit to E → T.

Move 2. Parse T. Cursor on ( → T → F, and F → ( E ). Eat (:

( ▸1 * 2 - 3 ) + 4 $

Move 3. F → ( E ) now parses the inner E. The cursor moves across 1 * 2 - 3 and comes out the other side:

( 1 * 2 - 3 ▸) + 4 $        inner E done

▎ Aside: strictly, this pseudocode can't parse 1 * 2 - 3 on its own either — the inner E and T hit the exact same "which clause?" conflict (on 1, is it T * F, T / F, or just F? — the three ? are indistinguishable). We grant the inner parse here so the spotlight stays on the top-level E decision, which is the one Appel is dissecting.

Move 4. Back in F → ( E ), eat the ):

( 1 * 2 - 3 ) ▸+ 4 $

Move 5. F done → our T → F done → our E → T done. We return to S, which insists on the end-marker: eat $. But the cursor is on +:

( 1 * 2 - 3 ) ▸+ 4 $
              ↑ expected  $ , found  +      ✗  SYNTAX ERROR
              
              
And this is how we end up with a parser error. Our previous grammar was arranged the way that allowed us to predict the future structure by just 1 token (the one we see first). This is not gonna happen in a more complex grammars.
