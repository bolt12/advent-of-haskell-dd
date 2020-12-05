# Advent of Haskell

Hey, everyone! Thank you for taking the time to read my blog post.
I hope you will learn something today and have a happy Christmas!

I just want to begin by saying a few things about what inspired me to write this post.
I have always been curious to understand the meaning of things, and I always find myself
noticing a lot of patterns and connections to mathematics in the computer engineering/science world.
Being a passionate functional programmer, I was able to experience this evidence more clearly,
but I feel like most people do not respect nor value these patterns.

I became increasingly concerned about program correctness and specification,
and what tools/methods were there to help one construct reliable/correct software.
Among several, the most recent one I contacted was Denotational Design (DD), which is precisely the
main theme of this blog post. When I first learned about DD, even though
I saw a huge potential in it, I didn't quite understand it, and I took a lot of time and reading
to process it all. Conal Elliott, the author of this method, was very kind and patient, and
answered a lot of my questions, which I have included in this article. Having said that,
I think it's hard for people to understand why you should care about things that Denotational
Design cares about, and even if you do understand, I find it hard to absorb all that this method has to offer.

There are not a lot of Denotational Design resources available, from which to read and learn on your own,
[and it may make you wonder if the method is really that relevant](https://www.quora.com/unanswered/Could-anyone-explain-the-process-of-Denotational-Design-that-was-invented-by-Conal-Elliott). However, a lot of people I admire,
respect and look up to, understand its importance and are, in certain respects, influenced by its ideas. Apart
from the [original paper](http://conal.net/papers/type-class-morphisms/), one of the few resources is Sandy Maguire's latest book, which I highly recommend if you enjoy this blog post. As you will see, in the end of this
post I present a collection of several other resources about DD, so stick until the end!

In view of this, in the hope of reaching more people on one of the topics that I consider
very important for every computer engineer/scientist to bear in mind, I decided to write this blog post,
which gives a very simple introduction to Denotational Design in a kind of Q&A format.
Let's hope you enjoy it as much as I do!

# Introduction

Denotational Design, developed by Conal Elliott, is an abstract and rigorous design method, that forces
forces the programmer to _really_ understand the nature of his
problem domain, and force him to step back and design the _meaning_ of abstraction before implementing it.
If the programmer is not able to correctly describe his abstraction to the machine,
he will introduce bugs, i.e. a leaky abstraction.
Denotational Design therefore gives us the ability to look at the designs and clearly ask whether
or not they are correct.

There are a lot of different programming languages available, each unique to its paradigm.
While it is possible to apply this method when using whatever language you choose,
purely functional programming languages are those that will be more suitable. "Why?", you may wonder.
Because, as you will see, if you want to be rigorous, meaningful and correct, you need to go through math,
and math is pure and functional. So, by creating abstractions using a purely functional programming language,
you can be very close to the actual specification. Finally, if you have a compiler that can guide you through
the design, pointing out the correct path of what you're trying to build, you'll be far more confident,
productive and successful.

With that being said, this post will be a very simple introduction to the Denotational Design method,
using Haskell. Hopefully, you can understand the motivation for this kind of methodologies,
as well as why it is important to care about the _meaning_ of programs in order to build non-leaky abstractions.

# Motivation

Abstraction leaks are at the very heart of a wrong decision and design. What's an
abstraction leak?

  **A:** Let's use an example outside Computer Science:

  For drivers, cars have abstractions. In its purest form, there is the steering wheel,
  the accelerator and the brake. This abstraction hides a lot of information about what's beneath the hood:
  engine, cams, timing belt, spark plugs, radiator, etc. The good thing about this abstraction is that we
  can substitute parts of the implementation with better parts without retraining the user.
  These adjustments improve performance, but the user keeps steering with the steering wheel and uses the pedals to
  start and stop. But there are leaks: in an automatic transmission, you can feel that the car loses power for
  a moment when the gears are switched; if the engine block is too cold, the car may not start or it may have
  poor performance, etc.

  An useful abstraction is one that gives us an understanding of the world that we accept as real.
  An outstanding abstraction is one that never reminds you of its falsehood.
  An useful abstraction profoundly changes the way you think and behave.

  So, an **abstraction leaks** when you still have to deal with details that are not part of the
  abstraction. That can arise when the implementation can't really conform to abstraction,
  or when the abstraction exposes too little details and you still have to work with the implementation
  specific details to make your program work.

  In software having leaky abstractions is so common that in 2002 Joel Spolsky coined
  the ["Law of Leaky Abstractions"](https://www.joelonsoftware.com/2002/11/11/the-law-of-leaky-abstractions/)
  that states: "All non-trivial abstractions, to some degree, are leaky." In his article
  Joel says that the law

  > means that whenever somebody comes up with a wizzy new code-generation
  tool that is supposed to make us all ever-so-efficient, you hear a lot of people saying
  "learn how to do it manually first, then use the wizzy tool to save time." Code generation
  tools which pretend to abstract out something, like all abstractions, **leak**, and the only
  way to deal with the leaks competently is to learn about how the abstractions work and what
  they are abstracting. **So the abstractions save us time working, but they don’t save us time learning.**

  This last sentence summarizes what to learn about this law and helps
  understanding the scope of this presentation. Paradoxically, although we have higher
  and higher level programming tools with increasingly better abstractions, becoming a
  proficient programmer is getting harder and harder. Producing non-leaky abstractions
  is possible if we are rigorous and our starting point is not by itself leaky. Math is
  all about abstraction, and it sure isn't leaky. This post aims to show how one is
  capable of producing non-leaky abstractions in software, by finding their meaning in
  math and then formulating (a) a representation that focuses on performance and (b) operations
  on that representation specified (not implemented) by a denotation function that requires that
  function to be _homomorphic_ over the designed API.

# Introduction

Data types have a central role in structuring our programs, whether checked statically or
dynamically. Adding data abstraction gives us a clean separation between a type's interface and
its implementation. Therefore, the **ideal abstraction** is as _simple as possible_, revealing everything the user
needs while hiding everything else (such as implementation details).
Purely functional programming languages allow the programmer to reason about code in a much more
straightforward (yet correct) way than do imperative programming languages, shifting the focus from _how_
to _what_ to do. Through purity and referential transparency,
the programmer is able to derive and calculate proofs and efficient programs. If aided by strong static
type systems, the programmer, can discharge a lot of correctness verifications to the compiler.
These are the main strengths of using a functional programming language, like Haskell, when
writing software, i.e., abstraction. If you are familiar with functional programming and
understand its value, you also understand that even with all these features it is hard to
design an interface that suits both the user and implementor without some experience or,
at least, notions of best practice. In the upcoming sections we will see how the Denotational Design
method ultimately leads to data abstraction/interface that connects implementors and users, while
still serving the distinct needs of each.

# Homomorphism Principle

Type classes provide a mechanism for varied implementations of standard interfaces.
Many of these interfaces are founded in mathematical tradition, thus having regularity not only of
types but also of properties (laws) that must hold. Type classes in Haskell
are known for having certain (implicit) laws that need to hold, such as the `Functor` or `Monad` type
classes. Haskellers rely on instances of these classes to abide by such laws in order
to reason and write code. 

The main value of laws in algebraic abstractions (“classes” in Haskell) is that they enable correct,
modular reasoning, in the sense that one can state, prove,
and assume properties of many different types at once. On another note, dependently typed
functional programming languages take these laws more seriously and hence achieve better dependability.
This is something that you don't see often in other languages or communities, and it may be the
reason why functional programmers are thought of producing more reliable abstractions.

The Denotational Design paper, advocates the Type Class Morphisms (TCM) principle, which
we will call the Homomorphism Principle (HP) during this article.
The idea is basically that, for a given type class, _the instances meaning follows the meaning's instance_.
This principle determines the required meaning of each type class instance,
thus defining the correctness of the implementation

# Denotational Design

On the previous section I said that the Denotational Design method ultimately leads to
an interface that is able to connect implementors and users, while still serving the distinct
needs of each.

- What kind of thing is an interface that can connect implementors and users while
still serving the distinct needs of each?

  **A:** Part of the answer is something that we can call "form" (which we can essentially
  understand as an API) which consists in the collection of data type names and operations
  that work on them. For example the interface of a finite map can be the following:

```haskell
  abstract type Map :: * -> * -> *

    empty :: (...) => Map k v
    insert :: (...) => k -> v -> Map k v -> Map k v
    lookup :: (Eq k, ...) => Map k v -> k -> Maybe v
```

  By itself the interface fails to serve the needs of the implementor and end-user.
  Although it hides implementation details it fails to _reveal_ a suitable substitute.
  More concretely:

  - Implementations reveal too much information to the user. Signatures ("forms") reveal too
  little;
  - An interface is _form without essence_;
  - The end-users **care** about the meaning of the names of the operations.

  In the example of the Map, nothing distinguishes that interface from another, besides
  syntactically:

```haskell
  abstract type Shoe :: * -> * -> *

    shoe :: (...) => Shoe a b
    littleShoe :: (...) => a -> b -> Shoe a b -> Shoe a b
    bigShoe :: (Eq a, ...) => Shoe a b -> a -> Maybe b
```

- What do we mean by "mean"? How can we give meaning/essence to our "form"?

  **A:** Denotational Semantics is an answer. The meaning of a data type is a mathematical
  object (Set, function, number, etc.). And the meaning of each operation is defined as
  the function that takes the meaning of its inputs to the meaning of its outputs.

  For our Map example, its meaning could be a _partial function_ from `k` to `v`. In this
  model, `empty` is the completely undefined function, `insert` extends the partial
  function and `lookup` is just function application.

  If we give the same meaning to the `Shoe` data type we can understand that the two
  distinct "forms" have the same essence, so we could replace one with the other.

In HP, type classes are meant to provide not only the "form" but also _part of the
essence_ of an interface's instance. This means that, for example, the `Monoid` type class, defines the
`mempty` and `mappend` operations and those operations need to satisfy the `Monoid` type
class laws. So, what about the **rest** of the meaning of a type class instance?

**A:** The **rest** of the meaning can be achieved by following the principle "the
instance's meaning follows the meaning's instance". In other words, the denotation is
_homomorphic_. In other words, the meaning of each operation application is given
by the application of the _same_ operation to the meaning of the arguments,
i.e. a type class morphism, preserving the class structure.

For the `Monoid` instance of the finite map type, the HP tells us that the
meaning of `mempty` on Maps must be the meaning of `mempty` for partial functions, and the
meaning of `mappend` of two Maps, must be the meaning of `mappend` for the partial functions
denoted by those Maps. Of course that, to use this principle, we need to know what `mempty` and `mappend`
mean for partial functions. On an unfortunate note, it happens that `mappend` isn't quite compatible with the
suggested denotation, which leads us to the following conclusion:

- Sometimes the HP property fails, and when it does, examination of the failure leads
to a simpler and more compelling design for which the principle holds. Denotational Design
by itself isn't able to tell us if it is the denotation that's wrong or if it is the design/"form" that's at
fault. But we should be thankfull however that we are able to notice our unfortunate choices!

_Homomorphisms_ are the key insight for the HP. A _homomorphism_ is a map between
two structures of the same type, that preserves the operations of the structures. Roughly,
it has this shape/pattern, depending on a given map `f` and operation `opN`:

```
f (a `op1` b) = f a `op1` f b
```

```
f (s `op2` v) = s `op2` f v
```

Type class morphisms or homomorphisms specify what correctness of implementation means.
If the homomorphism properties hold, then the implementation behaves like the semantics.
Therefore, users of a library can think of your implementation as if it were the semantics
(even though the implementation might be quite different), i.e. no abstraction leaks.
Basically, this means that if we follow homomorpism specification, the mental model used
by the end-users can be expected to always hold.

That's the main ingredient for the HP and adopting it might require additional up-front
effort in clarity of thinking, however the reward is that the resulting designs are simple and general,
and sometimes have the feel of profound _inevitability_.

# Stack Example

## Notation

`⟦ · ⟧` is the denotation function. So, in the light of the previous section, you can
see how the denotation function relates to the `Map` data type, as well as to its "form":

- `⟦ · ⟧ :: Map k v -> (k -> v)`
- `⟦ empty ⟧        = ⊥`
- `⟦ insert k v m ⟧ = \k' -> if k == k' then v else ⟦ m ⟧ k'`
- `⟦ lookup k m ⟧  = ⟦ m ⟧ k`

Denotation homomorpism (in pseudo Haskell):

```haskell
-- semantic
instance Monoid v => Monoid (Map k v) where
  ⟦ mempty ⟧ = mempty
  ⟦ ma `mappend` mb ⟧ = ⟦ ma ⟧ `mappend` ⟦ mb ⟧
```

## Stack

In order to be able to write a library that allows the end-user to create and manipulate
stacks, the implementor needs to fully understand what a stack is and what types of
operations that work on stacks make sense. Since stacks and their operations are common
knowledge amongst programmers, let's just dive write into the "form" or API:

```haskell
abstract type Stack :: * -> *

empty :: (...) => Stack a
push  :: (...) => a -> Stack a -> Stack a
pop   :: (...) => Stack a -> (a, Stack a)
```

Now, what is the essence of our "form"? What's the meaning of a `Stack` data type as well
as its operations? One could think "A stack is a linked list!" or "A stack is a LIFO
queue!", but **what is** a _list_ or a _queue_? What's the meaning of those structures, how
can we capture their essence? We quickly realise that we are not sure how to answer these
questions, and that we **do not** know what is the _essence_ of a stack.

Note that we could argue that a list is just a programming language primitive, such as
`Array`, `Vector` or `[]`, and that is fine. However, those solutions seem tainted with
details that are not specific to the domain in question, namely implementation details.

Understanding the problem at hand is the most important and harder part of designing a
software. How can one expect to be able to come up with a correct implementation whilst, at
the same time, offering a nice non-leaky abstraction to the end-user, without having full
comprehension of the problem domain?
Again, this is the most important and harder part of a Software Engineer's job:
understanding the problems so well that you can explain them to uncomprehending computer machines.

I argue that the simple essence of a `Stack` can be captured by the partial function that
maps natural numbers to elements in the stack:

```haskell
⟦ . ⟧ :: Stack a -> (Nat -> a)
```

The intuition behind this is that `(Nat -> a)` only captures the essence of what I think
makes a stack: a map between natural numbers (positions in the stack) and elements in the
stack. Given this, we have the following denotation on the operations:

- `⟦ empty ⟧    = ⊥`
- `⟦ push a s ⟧ = \n -> if n == 0 then a else ⟦ s ⟧ (n - 1)`
- `⟦ pop s ⟧    = (⟦ s ⟧ 0, \n -> ⟦ s ⟧ (n + 1))`

Please note that there will possibly be some potential for change in this denotation,
so please do not be too hung up on this suggestion. The main thing I want you to take away
is that the more time you spend trying to understand the problem, the better a suitable
denotation you can come up with!

### Type classes

Type classes provide a handy way to package up parts of an interface via a standard
vocabulary. Typically, a type class also has an associated collection of rules that
must be satisfied by instances of the class. In Haskell it is convenient if you give
additional operations on your data type such as the ones needed for `Functor` or `Applicative`.
By doing so, you not only discover extra structure for your data type, allowing you to
validate that the denotation you picked is indeed a good one, but also enrich your library
with more power and expressibility. Specially in Haskell a lot of type classes encapsulate
ubiquous patterns which translate to exceptionally well-studied mathematical objects, such
as `Monoid`s, `Lattice`s, etc. By recognizing these universal algebras in our designs, we will
end up with more powerful abstractions.

#### Functor

With that being said, it would be useful to make our `Stack` an instance of `Functor`, since
it's a nice thing to offer to the end-user. By using the HP we can see what it
means for a `Stack` to be a `Functor`, since the homomorpism properties must hold:

```haskell
-- semantic
instance Functor Stack where
  ⟦ fmap f s ⟧ = fmap f ⟦ s ⟧
```

This means that `fmap`ping a `Stack` can be
understood as `fmap`ing the partial function which denotes it. In other words
`fmap f` applies `f` to every element in the stack.

Now, `Functor` as 2 laws that need to hold:

- Identity: `fmap id = id`
- Composition: `fmap (f . g) = fmap f . fmap g`

Let's see how the laws hold:

```haskell
-- semantic
instance Functor Stack where
  ⟦ fmap f s ⟧ = \n -> f (⟦ s ⟧ n)
                 -- = f . ⟦ s ⟧

-- Laws:
-- ⟦ fmap id s ⟧       =
-- \n -> id (⟦ s ⟧ n)  =
-- \n -> ⟦ s ⟧ n       =
-- ⟦ s ⟧

-- ⟦ fmap (f . g) s ⟧          =
-- \n -> (f . g) (⟦ s ⟧ n)     =
-- \n -> f (g (⟦ s ⟧ n))       =
-- \n -> f ⟦ fmap g s ⟧ n      =
-- \n -> ⟦ fmap f (fmap g) ⟧ n =
-- ⟦ fmap f . fmap g ⟧
```

We could just give a sensible definition for `fmap` first and then see if the laws would
hold. Quickly we'd realise that the `Functor` instance definition for functions is just
function composition `(.)` and, indeed our `Functor` definition for `Stack` is a `Functor`
homomorpism!

#### Polishing

Let's simplify our denotation and make the partiality explicit in the
types. This way we'll avoid having error exceptions in our runnable specification, and type
safety is always good to have!

```haskell
abstract type Stack :: * -> *

empty :: (...) => Stack a
push  :: (...) => a -> Stack a -> Stack a
pop   :: (...) => Stack a -> (Maybe a, Stack a)

⟦ . ⟧ :: Stack a -> (Nat -> Maybe a)

⟦ empty ⟧    = const Nothing
⟦ push a s ⟧ = n -> if n == 0 then Just a else ⟦ s ⟧ (n - 1)
⟦ pop s ⟧    = (⟦ s ⟧ 0, (\n -> ⟦ s ⟧ (n + 1)))
```

This change requires us to revisit our `Functor` instance:

```haskell
instance Functor Stack where
  fmap f s = \n -> fmap f (s n)
        -- = fmap f . s
```

This is not a `Functor` morphism! This failure looks like bad news. Must we abandon
the HP, or does the failure point us to a new, and possibly better, model
for `Stack`? The rewritten semantic instances above do not make use of any properties
of `Stack` other than being a composition of two functors for the `Functor` instance.
So  let’s  generalize:

```haskell
⟦ . ⟧ :: Stack a -> (Compose ((->) Nat) Maybe a)
```

It may look like we’ve just moved complexity around, rather than eliminating it.
However, type composition is a very reusable notion, which is why it was already defined,
along with supporting proofs, that the `Functor` laws hold.

#### Calculating an implementation
 
##### Deriving Type Class Instances

Let's talk about efficient implementations/representations of a `Stack`. Until now all
we've done was to specify the precise denotation that characterises a `Stack`, which was a
partial map from the positions on the stack and the respective elements. Given this
semantics, we actually coded a runnable specification of it, applying the HP
along the way in order to further polish our abstraction and making sure it does not leak.

There might be cases where the runnable specification suits our needs, performance wise,
however there might be cases where it does not. In those cases it helps to be able to
calculate a more efficient representation without accidentaly introducing an abstraction
leak. For that effect, imagine we have the following type class:

```haskell
class IsStack s where
  -- mu = ⟦ . ⟧
  mu :: s a -> Stack a
  -- mu' = ⟦ . ⟧⁻¹
  mu'  :: Stack a -> s a

  -- mu' . mu = id
```

`mu` is our `⟦ . ⟧` semantic function and `mu'` is it's inverse.

Now, consider the `Functor` morphism property:

`mu (fmap f s) = fmap f (mu s)`

Because `mu . mu' = id`, the property is satisfied if:

`mu' (mu (fmap f s)) = mu' (fmap f (mu s))`

And because `mu' . mu = id`:

`fmap f s = mu' (fmap f (mu s))`

And, _by construction_, `mu` is a `Functor` morphism. Assuming the class laws
hold for `s`, they hold as well for `Stack`.

If `mu` and `mu'` have implementations, then we can stop here. Otherwise,
if we want to optimize the implementation, we can do some more work,
to rewrite the synthesized definitions.

##### List example

When trying to come up with the essence of a `Stack` we mentioned lists. Let's assume that
for whatever reason (I didn't performed any benchmarks) using lists is more efficient. Then:

```haskell
instance IsStack [] where
  mu [] = empty
  mu (h : t) = push h (mu t)

  mu' (S (Compose s)) = aux s 0
   where
    aux fn n = case fn n of
      Nothing -> []
      Just a  -> a : aux fn (n + 1)
```

Now, the equation:

`fmap f s = mu' (fmap f (mu s))` simplifies:

```haskell
-- [ a ] always "Just" values.

-- fmap f s = mu' (fmap f (mu s))
-- 
-- mu' (fmap f (mu s)) ==
-- < case splitting >
-- == { mu' (fmap f (mu []))
--    { mu' (fmap f (mu (h : t)))
-- < def - mu >
-- == { mu' (fmap f empty) }
--    { mu' (fmap f (push h (mu t)))
-- < def - Stack fmap x2; def - push x2 >
-- == { mu' (const Nothing) }
--    { mu' (push (f h) (mu t)) }
-- < always returning nothing = always returning empty list; def - mu' for Just values >
-- == { [] }
--    { f h : mu' (mu t) }
-- < mu' . mu = id >
-- == { [] }
--    { f h : t }

-- Result:
-- fmap f []    = []
-- fmap f (h:t) = f h : t
```

**Exercise**: Calculate implementations for `push` and `pop`.

# Author's personal insight

It all comes down to knowing whatever you're going to build/design.
Software developers have come a long way in writing programs without this approach,
simply by having a combination of truly knowing the issue at hand and having the necessary
knowledge and experience about how to communicate their understanding to the computer,
through a programming language.
While the HP doesn't seem to be as practical, it requires a great deal of effort up-front
and it gives beautiful results.

I'm not an advocate that software engineering is an art and that writing code should be thought
of as being written under some sort of romantic inspiration. For me, Software engineering should
be all about method, precision and correctness, just like every other engineering.
However, I know that there is a lot of flexibility, style and taste required to design programs,
also just like any other engineering. With software, I think we should be original and imaginative when
we come up with various abstractions, and there's definitely no science to come up with a fine,
simple denotation. But once we have one, concentrating on compositionality,
formal properties and precision would significantly improve the quality and reliability of our product.

Then again, it's all about knowing the problem in question, coming up with a meaningful denotation
and relying on a rigorous method, such as DD, to guide you in the search for the
essence of what you're trying to do.

# Insight Conal

- By cleanly separating programming interface and specification from implementation,
connecting the two by semantic homomorpism, one can achieve very elegant, generic,
simple and performant implementations.

- A goal of the Denotational Specification is to remove all operational/implementation bias
and get to the essential and elegant mathematical ideas. Then formulate a representation
that focuses on performance and operations on that representation specified in a
straightforward and regular way by a denotation function that requires that
that function to be homomorphic over the API/vocabulary. This is what
software/hardware design and implementation is all about.

- **Software design is the posing of tasteful algebra problems; and software implementation
is the correct solution of those problem.**

- **Software developers mostly lack these fundamental principles and so cannot distinguish
fundamental choices from inevitable consequences.**

- The sort of tension between model simplicity and expressiveness has been a source of deep
insights for me. The denotational design discipline brings these tensions to the surface
for examination. While lack of this discipline allows them to continue to be left unquestioned,
I count on raising this tension to help determine whether I’m right or wrong here. Most of what
people accept in programming is just bad taste left unquestioned and with very expensive consequences.

- If you pick a bad denotation, you’ll get bad results. A good denotation is one that captures the
essence of an idea (“a problem domain” in software design lingo) simply, precisely, and generally.
It clarifies our thinking about the domain, even before we’ve tried to relate it to representations
and implementations. Any operational bias will interfere with these goals. By “bad results” I mean weak
insight, complex implementation proofs and calculations, limited flexibility, and limited capability.

- Without the denotational discipline, we don't even have the mental lens through which to notice
unfortunate choices. One cannot even see the missed target and so cannot improve one's aim.

# Q&A Conal

- The homomorphism requirement is just so that we do not build leaky abstractions, right?

    **A:** I wouldn’t say “just” here. The homomorphism requirement is the specification,
    so it’s there to define your intent and thus to establish what correctness of implementation means
    (faithfulness to the specification). If homomorphism properties hold, then the implementation behaves
    like the semantics, so users of your library can think of your implementation as if it were the semantics
    (even though the implementation might be quite different), i.e., no abstraction leak.

- Should we stick only to one denotational semantic or can we have other when convenient?

    **A:** There may be exceptions to having just a single denotation, but if we do, all conversations
    will have to become much more explicit about which denotation.

    We can, and should, however consider several representations all explained in terms of the 
    single denotation. And then we have a rigorous, common basis for comparison.

- What if we lack the knowledge about what denotation to use? Or use the "wrong" denotation.

    **A:** Then we cannot design a good API and know what it means to correctly implement it.
    Denotation is the one overwhelmingly important creative choice. It takes good taste and
    what my math profs refer to as “mathematical maturity”.

    A good denotational model is optimized for precise simplicity, stripped of any
    implementation/efficiency (or even computability) bias.

- **How would you approach a problem for which you weren't sure what denotation would be best?**

    **A:** First, if I don’t know of a denotation, then I don’t understand the thing I’m trying
    to program. One cannot program well without understanding. It’s like engineering without
    science or physics without math. So, what I do is contemplate and study. Fortunately,
    good denotations are much simpler than good implementations and more enlightening.

- What distinguishes a good denotation from a "bad" one?

    **A:** I compare them using some basic criteria: The denotation must be precise, and adequate
    for what I want to express (but usually not the way I or others have been taught to express it).

- Why care about type class morphisms?

    **A**: I want my library’s users to think of behaviors and future values as being their semantic models.
    Why? Because these denotational models are simple and precise and have simple and useful formal properties.
    Those properties allow library users to program with confidence, and allow library providers to make radical
    changes in representation and implementation (even from demand-driven to data-driven) without
    breaking client programs.

# Finishing words

Thank you very much for your attention, I hope this blog post was enjoyable to you!
Most importantly, I hope it described the topic of Denotational Design in a simple and
intuitive manner. All feedback is welcome as I plan to keep polishing this article and write more about the intersection of denotational and formal methods. In the last section I have a collection of references and resources about the topic which I recommend having a look, if it peaked your interest!

Lastly, I just want to thank to the people that are organizing Advent of Haskell for the
spotlight and to the people that took their time to read and review this blog post, in
particular to Conal Elliott.

# References & Resources

In this section I gathered a lot of resources and references that I used to write this
post. I hope this can be seen as a contribution for newcomers that just heard about
DD for the first time! It has a lot of discussion on wether this method
is good or not so you can make your own choice! Thank you once again for your attention!

- [https://stackoverflow.com/questions/3883006/meaning-of-leaky-abstraction](https://stackoverflow.com/questions/3883006/meaning-of-leaky-abstraction)
- [Calculating compilers](https://github.com/conal/talk-2020-calculating-compilers-categorically#readme)
- [Denotational Design](http://conal.net/papers/type-class-morphisms/)
- [Algebra Driven Design](https://algebradriven.design/)
- [Semantics Design](https://lukepalmer.wordpress.com/2008/07/18/semantic-design/)
- [http://conal.net/blog/posts/simplifying-semantics-with-type-class-morphisms](http://conal.net/blog/posts/simplifying-semantics-with-type-class-morphisms)
- [https://poddtoppen.se/podcast/694047404/the-haskell-cast/episode-9-conal-elliott-on-frp-and-denotational-design](https://poddtoppen.se/podcast/694047404/the-haskell-cast/episode-9-conal-elliott-on-frp-and-denotational-design)
- [https://reasonablypolymorphic.com/blog/follow-the-denotation/](https://reasonablypolymorphic.com/blog/follow-the-denotation/)
- [https://lispcast.com/why-do-i-like-denotational-design/](https://lispcast.com/why-do-i-like-denotational-design/)
- [https://ro-che.info/articles/2014-12-31-denotational-design-does-not-work](https://ro-che.info/articles/2014-12-31-denotational-design-does-not-work)
- [https://lukepalmer.wordpress.com/2008/07/18/semantic-design/](https://lukepalmer.wordpress.com/2008/07/18/semantic-design/)
- [http://conal.net/blog/posts/denotational-design-with-type-class-morphisms](http://conal.net/blog/posts/denotational-design-with-type-class-morphisms)
- [https://wadler.blogspot.com/2009/02/conal-elliot-on-type-class-morphisms.html](https://wadler.blogspot.com/2009/02/conal-elliot-on-type-class-morphisms.html)
