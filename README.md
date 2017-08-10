pijul test
----------


Here we attempt to emulate *an assembly of heroes writing a poem* using `pijul` as their Version
Control System. Each of the heroes would be given a directory named after them, and initialize a
`pijul` repo therein first. Afterwards, they would perform some edits, pulling from the other
members of the fellowship right before and recording a patch just after each modificaion.

For now, there are two heroes: Robert & Alicia. They are tasked with recording a poem by Robert
Burns to a shared file, verse by verse.

TODO: Eventually I can introduce the notion of `action` that can be either changing a file, adding
a new one or moving things around. I would then generate a random sequence of such actions, each
associated with some hero or another -- a handful of such sequences, for good measure. Thus, many
possible operations on a repo would be performed and, hopefully, errors discovered. Maybe I could
even compare the resulting file hierarchy with the one that just applies the same actions
sequentially in one place, without the use of `pijul`.
