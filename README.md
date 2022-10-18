# CS316 “Functional Programming”

Welcome to the source code repository for the University of
Strathclyde CS316 “Functional Programming” course.

This is a course designed to teach Haskell to undergraduate
students. The written course materials are available from this
repository. Video lectures and access to the Mattermost forum for this
course are available to Strathclyde students via the course's [MyPlace
page](https://classes.myplace.strath.ac.uk/course/view.php?id=15897).

## Getting started

The code in this repository is structured as a
[Stack](https://docs.haskellstack.org/en/stable/README/) project. You
can install Stack by following the instructions on the linked
page.

### VSCode

The recommended text editor is Microsoft's Visual Studio Code. This
has a nice Haskell extension that will do syntax highlighting, type
checking and autocomplete for you. It also comes with a built-in Git
implemenation, so you don't have to install that separately. I have
prepared a short video that shows how to set up VS Code once you have
it and Stack installed: [CS316 : Getting started with VSCode
(03:22)](https://web.microsoftstream.com/video/782a862c-92ee-458f-951e-d7b59a1f9e44)
(Strathclyde students only).

### By hand

If you don't want to use VSCode, or want to know what is going on
behind the curtain, then you can clone and build the repository
yourself. Here are the commands that you would type in on a Unix-like
machine:

```
$ cd some/nice/directory      # replace 'some/nice/directory' with a real one
$ git clone https://github.com/bobatkey/CS316-2022.git
$ cd CS316-2022
$ stack build
    ... lots of output, might download some things ...
$ stack exec hello-cs316
hello CS316!
$ stack ghci lecture-notes/Week01.hs
    ... will start the interactive Haskell compiler with Week 1's lecture notes loaded ...
```

## Syllabus and Lecture Notes

The lecture notes for this course are intended to accompany the video
lectures, and provide mostly the same information in a searchable,
accessible and less bandwidth hungry format.

The notes are Haskell files with interleaved code and commentary. You
are encouraged to experiment by loading these files into `ghci` and
editing them. Each week also has a set of tutorial questions that you
should have a go at before the tutorial sessions on Fridays. The
solutions will be released after the session.

- [Week 1](lecture-notes/Week01.hs) : Data and Functions
  - [Tutorial Problems](lecture-notes/Week01Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week01Solutions)
  - [Live Lecture notes (Tuesday)](lecture-notes/Week01Intro.hs)
  - [Live Lecture notes (Friday)](lecture-notes/Week01Intro02.hs)
- [Week 2](lecture-notes/Week02.hs) : Solving Problems by Recusion
  - [Tutorial Problems](lecture-notes/Week02Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week02Solution.hs)
  - [Live Lecture Notes (Friday)](lecture-notes/Week02Intro.hs).
- [Week 3](lecture-notes/Week03.hs) : Higher Order Functions
  - [Tutorial Problems](lecture-notes/Week03Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week03Solutions.hs)
  - [Live Lecture Notes](lecture-notes/Week03Intro01.hs)
- [Week 4](lecture-notes/Week04.hs) : Patterns of Recursion
  - [Tutorial Problems](lecture-notes/Week04Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week04Solutions.hs)
  - [Live Lecture Notes (Tuesday)](lecture-notes/Week04Intro.hs)
- [Week 5](lecture-notes/Week05.hs) : Classes of Types
  - [Tutorial Problems](lecture-notes/Week05Problems.hs)
- [Week 6](lecture-notes/Week06.hs) : Simulating side-effects: Exceptions, State, and Printing
- [Week 7](lecture-notes/Week07.hs) : Monads
- [Week 8](lecture-notes/Week08.hs) : Real I/O and Parser Combinators
- [Week 9](lecture-notes/Week09.hs) : Data Dependencies and Applicative Functors
- [Week 10](lecture-notes/Week10.hs) : Lazy Evaluation and Infinite Data

You can take a look at [last year's
repository](https://github.com/bobatkey/CS316-2021) and [the one
before that](https://github.com/bobatkey/CS316-2020) for similar notes
and some different exercises.
