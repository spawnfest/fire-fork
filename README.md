FireFork
========

This is application for launching fireworks remotely.
Project created during SpawnFest 2017.


Idea behind project
---------------------

There is well known sweet spots for Erlang like asinchronous
communications, multiple parallel processes, distributed processing.. etc.

This time we wanted to try something different.
One friend told he has problem of launching fireworks synchrnized with audio.

So we decided to build system for this using erlang and have some fun :)


Technologies Used
=================


The system consists of several nodes:

  * station - represents a front-end node. It provides a GUI that interacts with an
    audio player and the stepper. The communitcation with a stepper is performed
    via radio (E32-TTL-100 module).

  * stepper - a node that acts as a controller for all the relays and relay controllers.
    It uses a script read from a csv file and controls relays via the GPIO interface.

Both of the above nodes are running on RaspberryPi 3 boards.
Apart from those, the system should have a node on a PC, that
provides administrative GUI for the system and a set of nodes
that runs on Arduino and acts as relay controllers. These nodes
were out of scope for the spawnfest.



Current status
==============

A prototype of the system is developed, but it is not working yet.


Problems
--------

  * Port drivers cannot be loaded for the gpio and the uart application
    in a release build using rebar3/relx. The same applications work well
    when started from a release build with rebar/reltool or when started
    from the interactive erlang node (erl). This issue was left unsolved.

  * Do not mix TX/RX wires when connecting Raspberry/Arduino with other
    devices, as it takes long time to track it down. :)


