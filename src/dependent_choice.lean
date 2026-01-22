-- -- If `α` has some distinguished element `x0 : α` and for every term `a : α` we can find `next a : α`
-- -- such that `R a (next a)` then we can find a sequence `x_ : ℕ → α` such that `∀ n, R (x_ n) (x_ (n+1))`
-- -- we do this by recursively applying `next` to the distinguished starting element
-- definition dependent_choice {α} (x0 : α) {R : α → α → Prop}
--                             (next : α → α)
--                             (next_spec : ∀ a, R a (next a)) : ℕ → α
-- -- Here we're using pattern matching to define the function.
-- -- If the input is 0 we return `x0`
-- | 0 := x0
-- -- If the input is of the form `n+1`, get the `n`th element `dependent_choice n` and then apply `next`
-- | (n+1) := next (dependent_choice n)

-- lemma dependent_choice_spec {α} (x0 : α) {R : α → α → Prop} (next : α → α)
--                             (next_spec : ∀ a, R a (next a))
--   : ∀ n, R (dependent_choice x0 next next_spec n) (dependent_choice x0 next next_spec (n + 1)) :=
-- -- This is literally the statement of next_spec!
-- λ n, next_spec (dependent_choice x0 next next_spec n)

-- -- The formulation of dependent choice looks a little silly, since `next` and `next_spec` are disconnected.
-- -- But we can package them up using the `my_subtype` type. 
-- -- The idea is that `my_subtype P` is the type of pairs `(a, h)` where `a : α` and `h : P a`
-- -- Since all proofs of a `Prop` are equal, the element `h` is "irrelevant", i.e. `(a, h) = (a, h')` for any `h h' : P a`
-- inductive my_subtype {α : Type} : (α → Prop) → Type
-- -- The Π means a "dependent function type". It's basically like ∀
-- -- So mk_pair takes an `a : α` and an `h : P a` and produces `mk_pair a h : my_subtype P`
-- | mk_pair : Π {P : α → Prop} (a : α), P a → my_subtype P

-- -- This type comes with two projection functions, like you'd expect for a pair.
-- definition my_subtype.fst {α} {P : α → Prop} : my_subtype P → α := 
-- -- Ignore the definition
--   λ x : my_subtype P,
--     my_subtype.cases_on x (λ Q y hy, y)

-- definition my_subtype.snd {α} {P : α → Prop} : ∀ (x : my_subtype P), P (my_subtype.fst x) := 
--   λ x : my_subtype P,
--     my_subtype.cases_on x (λ Q y hy, hy)

-- lemma my_subtype.canonical_form {α}
--   : ∀ {P : α → Prop} (x : my_subtype P),
--       x = my_subtype.mk_pair (my_subtype.fst x) (my_subtype.snd x) :=
--   by { intros, cases x, refl }

-- -- Now we can formulate dependent choice in a way that looks a little closer to the classical version
-- definition dependent_choice' {α} (x0 : α) {R : α → α → Prop}
-- -- Now instead of having a function `next` and a separate function `next_spec`,
-- -- we have a single function `H` such that `H a = my_subtype.mk_pair b h` for some `b : α` and `h : R a b`
-- -- Remember function in lean are curried, so `R a` is the function `b ↦ R a b` (also written `λ b, R a b`)
--                              (H : Π (a : α), my_subtype (R a)) : ℕ → α
-- | 0 := x0
-- | (n+1) := my_subtype.fst (H (dependent_choice' n))

-- lemma dependent_choice_spec' {α} (x0 : α) {R : α → α → Prop}
--                              (H : Π (a : α), my_subtype (R a))
--   : ∀ n, R (dependent_choice' x0 H n) (dependent_choice' x0 H (n + 1)) :=
-- -- This is still just packaged into the type of H
-- λ n, my_subtype.snd (H (dependent_choice' x0 H n))

-- -- But dependent choice is much stronger than this. Everything we've done so far is constructive!
-- -- The step needed in lean is to go from `Π a : α, exists (R a)` to `Π (a : α), my_subtype (R a)`.
-- -- Note that `exists (R a)` is what `∃ b : α, R a b` desugars to.
-- -- We can either add this as an axiom
-- axiom my_choice {α} {P : α → Prop} : exists P → my_subtype P

-- -- Or prove it using the lemmas in lean's classical library
-- -- But these are also derived from an axiom, which exists by fiat!
-- noncomputable
-- definition choice' {α} {P : α → Prop} (h : ∃ a : α, P a) : my_subtype P :=
-- my_subtype.mk_pair (classical.some h) (classical.some_spec h)

-- -- The type `subtype P` is similar to `∃ a : α, P a`, but it differs in one crucial way.
-- -- Since `(∃ a : α, P a) : Prop` and all inhabitants of a `Prop` are equal,
-- -- we can't actually produce an `a : α` such that `P a` from `∃ a : α, P a`. 
-- -- E.g. if `α ≝ ℤ` and `P n ≝ n * n = 4`, we could have proofs
-- -- `h ≝ ⟨2, ...⟩ : ∃ a : α, P a` and `h' ≝ ⟨-2, ...⟩ : ∃ a : α, P a` and `h = h'`.
-- -- But `2 ≠ -2` so there's no way to recover `2` from `h` or `-2` from `h'`.

-- -- noncomputable
-- -- definition classical.dependent_choice {α} [I : nonempty α] {R : α → α → Prop}
-- --   (H : ∀ (a : α), ∃ (b : α), R a b) : ℕ → α :=
-- --   @dependent_choice α (classical.inhabited_of_nonempty I) R
-- --                       (λ a, classical.subtype_of_exists (H a))

-- -- lemma classical.dependent_choice_spec {α} [I : nonempty α] {R : α → α → Prop} (H : ∀ (a : α), ∃ (b : α), R a b)
-- --   : ∀ n, R (classical.dependent_choice H n) (classical.dependent_choice H (n + 1)) :=
-- --   @dependent_choice_spec α (classical.inhabited_of_nonempty I) R (λ a, classical.subtype_of_exists (H a))