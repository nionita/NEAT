# [NEAT][]

This is a Haskell implementation of the NEAT method for evolving neural networks
through augmenting topologies, as described in the paper by Kenneth O. Stanley
and Risto Miikkulainen in the MIT Press Journall in 2002,
<http://mitpress.mit.edu/journals>.

The implementation has a few differences to the original NEAT, most important one
beeing that the generated networks are not recurrent (no loops in the network
connections). This restriction can be easily compensated by appropriate design
of the problem (i.e. some outputs serve as a state, which can be saved and used
as input in the next step).

[NEAT]: https://github.com/nionita/NEAT
