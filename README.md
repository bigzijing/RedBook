# RedBook

This repo is me working through the exercises in the book [Functional Programming in Scala](https://www.manning.com/books/functional-programming-in-scala) by Paul Chiusano and Runar Bjarnason (commonly known as The Red Book within the Scala community).

This repo is cloned from the [official GitHub](https://github.com/fpinscala/fpinscala/tree/first-edition), and then trimmed down to just the `exercises` folder as I iteratively work through them. The exercises also contain test suites written by a @philipschwarz from his [own fork](https://github.com/philipschwarz/fpinscala) of the official repo. Full credits for the test suite to him (as far as I can tell), they're very useful in making sure that my own solutions are correct and error-free (sometimes you miss an edge case or leave out something).

Originally this repo was a directory of [Ammonite](http://ammonite.io/) (.sc extension) scripts that I checked for correctness using Ammonite REPL, but that proved to be messy and tedious as the exercises grew to be more complex in structure and nature. 
I thus decided to turn to clone the official repo and use their existing structure which includes a sbt project structure, allowing an easier exercise checking workflow.

**I am following the *first edition* of the book as my workplace is still using Scala 2.13._ and Scala 3 has too big of a syntax change for me to comfortably and consistently learn at the moment due to a busy schedule and life.**


### How to use
- `exercises` are cloned from the official repo; function, trait, class definitions, etc are set to `???` (basically syntactic sugar to stub out an unimplemented code block)
- these `???` blocks would be implemented as I work along
- to check for correctness, `cd` into the `src/main/scala/[chapter]` and then run `sbt run`

**Note: the official repo removed the chapter names from the folders, i.e. chapter 3 on Functional Data Structures is just named `datastructures` on the repo; for my repo, I prefix-ed the chapter number, where applicable, for easier lookup**
