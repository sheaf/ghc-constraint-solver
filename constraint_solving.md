<!--
Compile the HTML slides with Pandoc:

pandoc -t revealjs -s -o index.html constraint_solving.md -V revealjs-url=reveal.js -V theme=welltyped_theme -V controlsTutorial=false -V controlsLayout=edges -V slideNumber='c/t' -V transition=slide

Custom theme in reveal.js/dist/themes/welltyped_theme.css
-->

---
author: Sam Derbyshire, Well-Typed
title: GHC's constraint solver
subtitle: Haskell eXchange
date: November 16th, 2021
---

<p align="center">
<img src=SPJ_french.svg height="500px" />
</p>

:::notes
Goal of typechecking (as per SPJ):

- Elaborate good programs.
- Reject bad programs with helpful error messages

Modular design: separate source Haskell from constraint language. 
:::

## Damas--Hindley--Milner

```haskell
foo xs = [ listToMaybe xs, Just (length xs) ]
```

<div class="fragment" data-fragment-index="1">
<div class="types">
<span class="fragment" data-fragment-index="2">`foo :: γ`<br></span>
<span class="fragment" data-fragment-index="3">`xs :: α`<br></span>
<span class="fragment" data-fragment-index="5">`(:) :: δ -> [δ] -> [δ]`<br></span>
<span class="fragment" data-fragment-index="7">`listToMaybe :: [ε] -> Maybe ε`<br></span>
<span class="fragment" data-fragment-index="9">`Just :: ζ -> Maybe ζ`<br></span>
<span class="fragment" data-fragment-index="9">`length :: [η] -> Int`</span>
</div>
<div class="worklist">
<p class="worklist-header">Work list</p>
<span class="fragment fade-out" data-fragment-index="11"><span class="fragment" data-fragment-index="10">`δ ~ Maybe ζ`<br></span></span>
<span class="fragment fade-out" data-fragment-index="11"><span class="fragment" data-fragment-index="10">`[η] ~ α`<br></span></span>
<span class="fragment fade-out" data-fragment-index="12"><span class="fragment" data-fragment-index="11">`Maybe ε ~ Maybe ζ`<br></span></span>
<span class="fragment fade-out" data-fragment-index="12"><span class="fragment" data-fragment-index="11">`[η] ~ [ε]`</span></span>
</div>
<div class="inerts">
<p class="inerts-header">Inert set</p>
<span class="fragment" data-fragment-index="4">`γ ~ α -> β`<br></span>
<span class="fragment" data-fragment-index="6">`β ~ [δ]`<br></span>
<span class="fragment" data-fragment-index="8">`α ~ [ε]`<br></span>
<span class="fragment" data-fragment-index="8">`δ ~ Maybe ε`<br></span>
<span class="fragment" data-fragment-index="10">`ζ ~ Int`<br></span>
<span class="fragment" data-fragment-index="12">`ε ~ ζ`<br></span>
<span class="fragment" data-fragment-index="12">`η ~ ε`</span>
</div></div>

<br><br><br><br><br><br><br><br>
<div class="infobox fragment" data-fragment-index="13">
This is **Algorithm W** from Damas--Hindley--Milner type theory.  
It always infers the most general type.
</div>

:::notes
`~` means equality. It is symmetric but we mostly mean it left-to-right.
Using Greek letters for metavariables, Latin letters for skolem variables.
:::

## Elaboration

```haskell
foo xs = [ listToMaybe xs, Just (length xs) ]
```

:::{.element: class="fragment"}
```haskell
foo :: Int -> [Maybe Int]
foo (xs :: [Int])
  = listToMaybe @Int xs
  : @(Maybe Int)
    Just @Int ( length @Int xs )
  : @(Maybe Int)
    [] @(Maybe Int)
```
:::

<!--
:::{.element: class="fragment"}
```haskell
f x = x
```
:::
:::{.element: class="fragment"}
```haskell
f :: ∀ a. a -> a
```
:::

:::{.element: class="fragment"}
<p align="center" class="infobox">
<img src=HM_gen.svg height="48px" style="margin-right:12px;margin-top:12px"/>
<img src=HM_inst.svg height="48px" style="margin-left:12px;margin-top:12px"/>
</p>
:::
-->

## Scaling up to Haskell

```haskell
type family F a where { F (Maybe a) = a }
g :: F a -> a -> a

f x y = [ g x y, Just (not x) ]
```

<div class="fragment" data-fragment-index="1">
<div class="types">
<span class="fragment" data-fragment-index="2">`f :: α -> β -> γ`<br></span>
<span class="fragment" data-fragment-index="3">`(:) :: δ -> [δ] -> [δ]`<br></span>
<span class="fragment" data-fragment-index="5">`g :: F ε -> ε -> ε`</span>
</div>
<div class="worklist">
<p class="worklist-header">Work list</p>
<span class="fragment fade-out" data-fragment-index="8"><span class="fragment" data-fragment-index="7">`α ~ Bool`<br></span></span>
<span class="fragment fade-out" data-fragment-index="10"><span class="fragment" data-fragment-index="7">`δ ~ Maybe Bool`<br></span></span>
<span class="fragment fade-out" data-fragment-index="11"><span class="fragment" data-fragment-index="9">`α ~ F ε`</span></span>
</div>
<div class="inerts">
<p class="inerts-header">Inert set</p>
<span class="fragment" data-fragment-index="4">`γ ~ [δ]`<br></span>
<span class="fragment fade-out" data-fragment-index="9"><span class="fragment" data-fragment-index="6">`α ~ F ε`<br></span></span>
<span class="fragment" data-fragment-index="6">`β ~ ε`<br></span>
<span class="fragment" data-fragment-index="6">`ε ~ δ`<br></span>
<span class="fragment" data-fragment-index="8">`α ~ Bool`<br></span>
<span class="fragment" data-fragment-index="10">`δ ~ Maybe Bool`</span>
</div>
</div>

:::notes
Note how we go between `forall a. F a -> a -> a` and `F ε -> ε -> ε`: generalisation and instantiation.
:::

<!--
## Bidirectional type-checking

<br>
<p align="center" class="typing">
<img src=infer.svg height="40px" style="padding-top:20px;padding-bottom:4px"/>
</p>

**Type inference**: we figure out the types of individual components, and put the pieces together
to get the type of the overall expression.

<br>

<p align="center" class="typing">
<img src=check.svg height="40px" style="padding-top:20px;padding-bottom:4px"/>
</p>

**Type checking**: we have an outer type signature, and we push it down to check that everything inside
is of the expected type.

##

```haskell
h f = (f Nothing, f True)
```

:::{.element: class="fragment"}
We don't infer a type for `h`, but we can check `h` at type

```haskell
h :: ∀ a. (∀ b. b -> b) -> (Maybe a, Bool)
```
:::

## Residual constraints

- Inferring: generalise.
- Checking: report an error.

Add some examples here.
-->

## Typeclasses, implications

```haskell
palindrome :: Eq a => [a] -> Bool
palindrome ds = ds == reverse ds
```

:::{.element: class="fragment"}
```haskell
palindrome :: Eq a => [a] -> Bool
palindrome = \ @a ($dEq_a :: Eq a) ->
  let
    $dEq_List_a :: Eq [a]
    $dEq_List_a = $fEq_List @a $dEq_a
  in \ (ds :: [a]) -> (==) @[a] $dEq_List_a ds (reverse @a ds)
```
:::

:::{.element: class="fragment"}
Given: `Eq a`  
Wanted: `Eq [a]`  
Solve the implication `[G] Eq a ⊢ [W] Eq [a]` using the dictionary function `$fEq_List :: Eq a -> Eq [a]`.
:::

:::notes
We denote Givens by `[G]` and Wanteds by `[W]`.
What's a Wanted and what's a Given is obvious here (Givens to the left of the implication, Wanteds to the right),
but it's useful to specifically notate this anyway.
:::

## Nested implications

```haskell
type family F a where { F Int = Int; F (f a) = a }

data G a where
  MkG1 :: Int -> G Int
  MkG2 :: Integral b => Maybe c -> b -> G (Maybe c)

foo :: Num (F a) => G a -> F a
foo x = case x of
  MkG1 i   -> i
  MkG2 m b -> fromMaybe (fromIntegral b) m
```

:::{.element: class="fragment"}
```haskell
[G] Num (F a) ⊢
    [ [G] a ~ Int ⊢ [W] F a ~ Int
    , ∀ b c. [G] Integral b, [G] a ~ Maybe c
           ⊢ [W] Integral b, [W] Num c ]
```
:::

## Part II: constraint solving

## Predicates


Different kinds of constraints have different kinds of evidence:

  - typeclass constraints have a dictionary of the methods,
  - an equality is witnessed by a *coercion* (proof term).

<br>

:::{.element: class="fragment"}
| Predicate   | Examples                                    | Evidence      |
| ----------- | ------------------------------------------- | ------------- |
| Typeclass   | `Ord a`, `Num a`, `(c1, c2)`, `()`, `a ~ b` | Dictionary    |
| Equality    | `a ~# b`, `a ~R# b`                         | Coercion      |
| Quantified  | `∀ a. Eq a => Eq (f a)`                     | Function      |
| Irreducible | `c a`, `F x y`                              | Not yet known |
:::

:::notes
A quantified constraint is like a simple implication constraint,
what's allowed is same as in an instance head, e.g. can't do `forall a. C a => C (F a)`.
:::


## Solving flat constraints

<p align="center">
<img src=interaction_pipeline.svg height="550px" />
</p>


<!--
## Instance resolution

```haskell
instance ctx => Cls ty_1 ... ty_n
```

:::{.element: class="fragment"}
When we try to solve `Cls a_1 ... a_n` by using instance, there should not be
any ambiguity about which instance to choose:

  - one instance must **match**, no others may **unify** (=potentially match later).
:::

## Instance resolution 2

```haskell
instance a ~ [Int] => C (Maybe a)
           -- VS
instance              C (Maybe [Int])
```

:::notes
If we have [W] C (Maybe [b]),
we will interact with the first instance
and pick up a new Wanted [W] [b] ~ [Int],
which canonicalises to `b ~ Int`.
:::

## Overlap

```haskell
instance                     C1 a
instance {-# OVERLAPPING #-} C1 (Maybe a)
```

<br>

<span class="fragment" data-fragment-index="2" style="color:FireBrick">✘</span> <span class="fragment" data-fragment-index="1">`[W] C1 a`</span>

<span class="fragment" data-fragment-index="3" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C1 (Maybe Int)`</span>

<span class="fragment" data-fragment-index="4" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C1 Int`</span>

<br>

:::{.element: class="fragment"}
Discard less-specific overlapped instances.
:::

## Overlap 2

```haskell
instance                     C2 a   b
instance {-# OVERLAPPING #-} C2 Int b
instance {-# OVERLAPPING #-} C2 a   Bool
instance {-# OVERLAPPING #-} C2 Int Bool
```

<br>

<span class="fragment" data-fragment-index="2" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C2 Bool Int`</span>

<span class="fragment" data-fragment-index="3" style="color:FireBrick">✘</span> <span class="fragment" data-fragment-index="1">`[W] C2 Int b`</span>

<span class="fragment" data-fragment-index="4" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C2 Int Int`</span>

<span class="fragment" data-fragment-index="5" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C2 [a] Bool`</span>

<span class="fragment" data-fragment-index="6" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C2 Int Bool`</span>

## Incoherence 1

```haskell
instance {-# INCOHERENT #-} C3 Int b
instance {-# INCOHERENT #-} C3 a   Bool
```

<span class="fragment" data-fragment-index="2" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C3 Int b`</span>

<span class="fragment" data-fragment-index="3" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C3 a Bool`</span>

<span class="fragment" data-fragment-index="4" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C3 Int Bool`</span>

<br>

:::{.element: class="fragment"}
If all matching instances are incoherent: pick any.
:::

## Incoherence 2

```haskell
instance                    C4 [a]   b   Int
instance {-# INCOHERENT #-} C4 [Int] b   c
instance {-# INCOHERENT #-} C4 a     Int c
```

<br>

<span class="fragment" data-fragment-index="2" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C4 [a] b Int`</span>

<span class="fragment" data-fragment-index="4" style="color:ForestGreen">✔</span> <span class="fragment" data-fragment-index="1">`[W] C4 [Int] Int Int`</span>



<br>

:::{.element: class="fragment" data-fragment-index="3"}
Allow other constraints to unify as long as they are all incoherent.
:::

:::{.element: class="fragment" data-fragment-index="5"}
If there's exactly one non-incoherent match, we can pick it if all unifiers are incoherent.
:::


:::notes
Incoherent: allows GHC to make a guess.
Can still get incoherence with overlapping instances across module boundaries:
we might typecheck a function in a certain context where we see no overlap,
but use it in a different module in which we would have rejected the definition
because of overlap of available instances.
:::
-->

## Rewriting

When we add a new equality `co :: old_ty ~ new_ty` to the inert set, we kick out
constraints that can be rewritten using `co`, adding them back to the work list to be processed again.

<br><br>

<div class="worklist2 fragment" data-fragment-index="1">
<span class="fragment fade-out" data-fragment-index="3"><span class="fragment" data-fragment-index="2">`[W] a ~ Maybe b`<br></span></span>
<span class="fragment fade-out" data-fragment-index="5"><span class="fragment" data-fragment-index="4">`[W] Eq a`<br></span></span>
</div>
<div class="inerts2 fragment" data-fragment-index="1">
<span class="fragment fade-out" data-fragment-index="4"><span class="fragment" data-fragment-index="2">`[W] $dEq :: Eq a`<br></span></span>
<span class="fragment" data-fragment-index="2">`[W] $dNum :: Num b`<br></span>
<span class="fragment" data-fragment-index="3">`[W] co :: a ~ Maybe b`<br></span>
<span class="fragment" data-fragment-index="5">`[W] $dEq |> Eq co :: Eq (Maybe b)`<br></span>
</div>

:::notes
I'm now writing down the evidence terms.
  - Wanteds do not rewrite Givens.
  - `~R#` does not rewrite `~#`.
  - W/N does not rewrite W/R (technical reason to ensure substitution is terminating, but see #19664).
:::

## Decomposition


```haskell
(a -> (b,c)) ~ (x -> (y,z))
  ⇝ a ~ x, (b,c) ~ (y,z)
  ⇝ a ~ x, b ~ y, c ~ z
```

:::{.element: class="fragment"}
```haskell
Nt a ~R# b
  ⇝ a ~R# b -- if the newtype constructor for Nt is in scope
```
:::

## Canonicalisation

A **canonical** constraint is one that is in **atomic** form: it can't be decomposed or rewritten in any way.

:::{.element: class="fragment"}
```haskell
type TyFamCt :: Type -> Constraint
type family TyFamCt ty where
  TyFamCt Bool  = ()
  TyFamCt (a,b) = a ~ b
```
:::

:::{.element: class="fragment"}
```haskell
TyFamCt (f a, g b)
  ⇝ f a ~ g b
  ⇝ f a ~# g b
  ⇝ f ~# g, a ~# b
```
:::

## Solving implications

<p align="center">
<img src=solving_implications.svg height="550px" />
</p>

##

```haskell
type family F a where { F Int = Int, F (f a) = a }
```

```haskell
[G] Num (F a) ⊢
    [ [G] a ~ Int ⊢ [W] F a ~ Int
    , ∀ b c. [G] Integral b, [G] a ~ Maybe c
           ⊢ [W] Integral b, [W] Num c ]
```

<div class="worklist2">
<p class="worklist-header">Work list</p>
<span class="fragment fade-out" data-fragment-index="3"><span class="fragment" data-fragment-index="2">`[G] co :: a ~ Int`<br></span></span>
<span class="fragment fade-out" data-fragment-index="5"><span class="fragment" data-fragment-index="2">`[W] F a ~ Int`<br></span></span>
<span class="fragment fade-out" data-fragment-index="6"><span class="fragment" data-fragment-index="4">`[G] $dNum :: Num (F a)`<br></span></span>
<span class="fragment fade-out" data-fragment-index="6"><span class="fragment" data-fragment-index="5">`[W] (F co ; F[0]) :: F a ~ Int`<br></span></span>
<span class="fragment fade-out" data-fragment-index="11"><span class="fragment" data-fragment-index="10">`[G] $dIntegral :: Integral b`<br></span></span>
<span class="fragment fade-out" data-fragment-index="11"><span class="fragment" data-fragment-index="10">`[G] co :: a ~ Maybe c`<br></span></span>
<span class="fragment fade-out" data-fragment-index="13"><span class="fragment" data-fragment-index="10">`[W] Integral b`<br></span></span>
<span class="fragment fade-out" data-fragment-index="13"><span class="fragment" data-fragment-index="10">`[W] Num c`<br></span></span>
<span class="fragment fade-out" data-fragment-index="14"><span class="fragment" data-fragment-index="12">`[G] $dNum :: Num (F a)`</span></span>
</div>
<div class="inerts2">
<p class="inerts-header">Inert set</p>
<span class="fragment fade-out" data-fragment-index="4"><span class="fragment" data-fragment-index="1">`[G] $dNum :: Num (F a)`<br></span></span>
<span class="fragment fade-out" data-fragment-index="9"><span class="fragment" data-fragment-index="3">`[G] co :: a ~ Int`<br></span></span>
<span class="fragment fade-out" data-fragment-index="9"><span class="fragment" data-fragment-index="6">`[G] ($dNum |> Num co) :: Num Int`<br></span></span>
<span class="fragment fade-out" data-fragment-index="12"><span class="fragment" data-fragment-index="9">`[G] $dNum :: Num (F a)`<br></span></span>
<span class="fragment fade-out" data-fragment-index="16"><span class="fragment" data-fragment-index="11">`[G] $dIntegral :: Integral b`<br></span>
<span class="fragment fade-out" data-fragment-index="16"><span class="fragment" data-fragment-index="11">`[G] co :: a ~ Maybe c`<br></span>
<span class="fragment fade-out" data-fragment-index="15"><span class="fragment" data-fragment-index="13">`[W] Num c`<br></span></span>
<span class="fragment fade-out" data-fragment-index="16"><span class="fragment" data-fragment-index="14">`[G] ($dNum |> Num (F co ; F[1])) :: Num c`</span>
</div>

:::notes
Note: we are dropping the Wanteds from the inert set once we fully solve them,
but we separately record the evidence as this is used in elaboration ("evidence bindings").
:::


## Questions?

Slides available online:  
&nbsp;&nbsp;[sheaf.github.io/ghc-constraint-solver](https://sheaf.github.io/ghc-constraint-solver)
