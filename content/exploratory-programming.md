+++
+++

In this page I gather all information I have written or gathered around
*exploratory programming*.

You can read what I mean by that in the [original
essay](./essays/explorative-programming) I wrote.

You can also check the
[Lobste.rs](https://lobste.rs/s/igkrfa/explorative_programming) and
[Hacker News](https://news.ycombinator.com/item?id=39835343)
discussions around that essay.

Several tools have been brought to my attentions around the publishing
of the essay:

## Interlisp

The original OS of the Xerox Lisp Machines has some interesting ideas
that are very similar to what I thought about. Mainly related to
avoiding the File-based and file organization problems.

It was able to keep track of what was modified and what was new and
update the files with the new definitions. They called this the
"resident mode". You can read more about this and other interlisp
goodies in
[these](https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=b8f849085d608a657eea30f771b5a6de204a7f3f)
[two](https://dl.acm.org/doi/pdf/10.1145/356715.356719) documents.

There's been a recent effort by the Medley Interslip organization to
port the original Interlisp into modern OSs, with people from the
original design involved. I'll write more about Interlisp in future
posts.

You can check Interlisp out on their [website](https://interlisp.org).
You can check the conversation where Interlisp was brought to my
attention
[here](https://elk.zone/functional.cafe/@amoroso@fosstodon.org/112163251479658005).
In the hacker news discussion there are also some references to Interlisp.
Make sure to check those as well :)

## Pharo

A lot of people commented that what I was talking about was very much what
Smalltalk offered in many of their Desktop environments.

I really need to expand more into this and learn more about
[Pharo](https://pharo.org/). I've tested Pharo before a couple of
years ago, and while very interesting the transition from prototype to
production didn't seem the most straightforward. I need to give it a
deeper try, particularly around the prototyping stage (Although being
able to move into production is something that is necessary for me,
since it is also another source of rigidness).

### GToolkit

Implemented in Pharo, [Glamorous Toolkit](https://gtoolkit.com/) is
something I really ought to give a try, it's some kind of
personalizable development environment, what they call a Moldable
Develeopment environment. Will be testing this soon and possibly write
an article about it! It looks incredibly interesting to develop custom
tooling around another language/system but as with Pharo, it cannot go
into production (to confirm).

It may be very interesting to draw inspiration from this though.
