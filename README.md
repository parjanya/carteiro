# carteiro

A system for querying the brazilian post office tracking system.

# Installation

For now, clone the repository on `~/quicklisp/local-projects/` and then load it by

`(quicklisp:quickload :carteiro)`.

(For `quicklisp`, check https://www.quicklisp.org/beta/.)

# Usage

`(carteiro:rastreio "[tracking code]")` will hopefully spit all that happened to you parcel so far.

Try `cu276167862IN` for instance.
