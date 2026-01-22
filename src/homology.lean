import algebraic_topology.Moore_complex
import algebraic_topology.simplicial_set
import algebra.category.Module.abelian
import algebra.category.Module.adjunctions
import algebra.homology.homology
import topology.homotopy.basic

open category_theory algebraic_topology

noncomputable
def singular_homology (n : ℕ) : Top ⥤ Module ℤ :=
  Top.to_sSet
  ⋙ ((simplicial_object.whiskering _ _).obj (Module.free ℤ))
  ⋙ normalized_Moore_complex _
  ⋙ homology_functor _ _ n

lemma singular_homology.homotopy_invariant (n : ℕ) (X Y : Top) (f g : X ⟶ Y) 
  (H : homotopy f g) : singular_homology.hom f = singular_homology.hom g

noncomputable
def temp := simplex_category.to_Top.obj (simplex_category.mk 1)