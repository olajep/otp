Erlang on Adapteva's Parallella
===============================

This is a modified version of the Erlang runtime system that can run Erlang on
the Epiphany co-processor, most commonly found on the [Parallella] [2]
single-board computer.

**Erlang** is a programming language used to build massively scalable soft
real-time systems with requirements on high availability. Some of its uses are
in telecom, banking, e-commerce, computer telephony and instant messaging.
Erlang's runtime system has built-in support for concurrency, distribution and
fault tolerance.

**OTP** is set of Erlang libraries and design principles providing
middle-ware to develop these systems. It includes its own distributed
database, applications to interface towards other languages, debugging
and release handling tools.

Please see the [erlang/otp] [3] repository for upstream Erlang/OTP.

ERTS and BEAM
-------------
**BEAM** is the name of the virtual machine where all Erlang code is executed.
Every compiled Erlang file has the suffix .beam. The virtual machine
is sometimes referred to as the emulator.

**ERTS** is the Erlang Runtime System where the BEAM, kernel and
standard libraries amongst others are included.

More information can be found at [erlang.org] [1].

Building and Running
--------------------
Building from a git clone is quite simple:

    ./otp_build autoconf
    ./configure --enable-slave-emulator --enable-hipe
    make -j4

You will get the `erl` binary in `bin/erl`. Start it up, and you will see
`[slave:0]` in the ERTS system version line. This means that you are not using
the Epiphany. Only one process may use it at a time. To do so, use:

    env SLAVE_BINARY=bin/epiphany-unknown-elf/slave.smp bin/erl

It should now show `[slave:16]` instead. Use `epiphany:spawn/1` to run an Erlang
process on the Epiphany.

There are some tests included. These can be run on the release build (that we
compiled above) with the following command:

    make ERL_TOP=$(PWD) -j4 -C tests fast

Copyright and License
---------------------

> %CopyrightBegin%
>
> Copyright Ericsson AB 2010-2014. All Rights Reserved.
>
> The contents of this file are subject to the Erlang Public License,
> Version 1.1, (the "License"); you may not use this file except in
> compliance with the License. You should have received a copy of the
> Erlang Public License along with this software. If not, it can be
> retrieved online at http://www.erlang.org/.
>
> Software distributed under the License is distributed on an "AS IS"
> basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
> the License for the specific language governing rights and limitations
> under the License.
>
> %CopyrightEnd%



   [1]: http://www.erlang.org
   [2]: https://www.parallella.org/
   [3]: https://github.com/erlang/otp
