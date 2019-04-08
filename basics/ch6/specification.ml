(* http://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/abstract/specification.html *)

(*

A specification is a contract between a client of some unit of code and the implementer of that code.
The most common place we find specifications is as comments in the interface (.mli) files for a module.
There, the implementer of the module spells out what the client may and may not assume about the module's behavior.
This contract makes it clear who to blame if something goes wrong: Did the client misuse the module?
Or did the implementer fail to deliver the promised functionality?

Specifications usually involve preconditions and postconditions.
The preconditions inform what the client must guarantee about inputs they pass in, and what the implementer may assume about those inputs.
The postconditions inform what they client may assume about outputs they receive, and what the implementer must guarantee about those outputs.

An implementation satisfies a specification if it provides the behavior described by the specification.
There may be many possible implementations of a given specification that are feasible.
The client may not assume anything about which of those implementations is actually provided.
The implementer, on the other hand, gets to provide one of their choice.

Good specifications have to balance two conflicting goals; they must be

sufficiently restrictive, ruling out implementations that would be useless to clients, as well as

sufficiently general, not ruling out implementations that would be useful to clients.

Some common mistakes include
  not stating enough in preconditions,
  failing to identify when exceptions will be thrown,
  failing to specify behavior at boundary cases,
  writing operational specifications instead of definitional and
  stating too much in postconditions.

Writing good specifications is a skill that you will work to master the rest of your career.
It's hard because the language and compiler do nothing to check the correctness of a specification:
there's no type system for them, no warnings, etc.
(Though there is ongoing research on how to improve specifications and the writing of them.)
The specifications you write will be read by other people, and with that reading can come misunderstanding.
Reading specifications requires close attention to detail.

Specifications should be written quite early.
As soon as a design decision is made, document it in a specification.
Specifications should continue to be updated throughout implementation.
A specification becomes obsolete only when the code it specifies becomes obsolete and is removed from the code base.

Clear specifications serve many important functions in software development teams.
One important one is when something goes wrong, everyone can agree on whose job it is to fix the problem:
either the implementer has not met the specification and needs to fix the implementation,
or the client has written code that assumes something not guaranteed by the spec,
and therefore needs to fix the using code.
Or, perhaps the spec is wrong, and then the client and implementer need to decide on a new spec.
This ability to decide whose problem a bug is prevents problems from slipping through the cracks

 *)

 (* How might we specify sqr, a square-root function?
  First, we need to describe its result.
  We will call this description the returns clause because
  it is a part of the specification that describes the result of a function call.
  It is also known as a postcondition:
   it describes a condition that holds after the function is called. Here is an example of a returns clause:
 *)

 (* no need to say "return" *)
 (* [sqr x] is the square root of [x]. *)

 (** [find lst x] is the index of [x] in [lst], starting from zero. *)


(* The specification for sqr doesn't completely make sense because the square root does not exist for some x of type real.
 The mathematical square root function is a partial function that is defined over only part of its domain.
  A good function specification is complete with respect to the possible inputs;
  it provides the client with an understanding of what inputs are allowed and what the results will be for allowed inputs.

We have several ways to deal with partial functions.
A straightforward approach is to restrict the domain so that it is clear the function cannot be legitimately used on some inputs.
 The specification rules out bad inputs with a requires clause establishing when the function may be called.
 This clause is also called a precondition because it describes a condition that must hold before the function is called.
 Here is a requires clause for sqr:
*)

(** [sqr x] is the square root of [x]. Its relative accuracy is no worse
    than 1.0x10^-6.  Requires: [x >= 0] *)

(* Makes no claims about what happens is precondition isn't satisfied - and doesn't have to. That can be up to the implementer *)


(* Another way to deal with partial functions is to convert them into total functions
(functions defined over their entire domain). T
his approach is arguably easier for the client to deal with because the function's behavior is always defined;
it has no precondition. However, it pushes work onto the implementer and may lead to a slower implementation.

How can we convert sqr into a total function?
 One approach that is (too) often followed is to define some value that is returned
 in the cases that the requires clause would have ruled; for example: *)

(* (** [sqr x] is the square root of [x] if [x >= 0],
    with relative accuracy no worse than 1.0x10^-6.
    Otherwise, a negative number is returned. *) *)

(* This practice is not recommended because it tends to encourage broken, hard-to-read client code.  *)

(* Still forces implementer to write if/else clauses when using this function *)

(* A better way to make functions total is to have them raise an exception when the expected input condition is not met.
Exceptions avoid the necessity of distracting error-handling logic in the client's code.
If the function is to be total, the specification must say what exception is raised and when.
For example, we might make our square root function total as follows:

(** [sqr x] is the square root of [x], with relative accuracy no worse
    than 1.0x10^-6. Raises: [Negative] if [x < 0]. *)
 *)

(* It can be useful to provide an illustrative example as part of a specification.
 No matter how clear and well written the specification is, an example is often useful to clients. *)

(** [find lst x] is the index of [x] in [lst], starting from zero.
    Example: [find ["b","a","c"] "a" = 1]. *)