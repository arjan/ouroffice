Who's at @ouroffice?
====================

This Erlang node monitors hostnames on the local network using `nmap`
and `arp`, and checks who is online. 

Installation
------------

First, make sure you have the `nmap` and `arp` tools available on your
computer.

Next, copy `sample.config` to `youroffice.config` and edit the values in the
file according to the people you want to track and the subnet you want
to ping.

Then, compile the Erlang code with rebar:

    rebar get-dep compile
    
Finally, start the scanner in development mode with:

    erl -pa ebin -pz deps/*/ebin -config youroffice.config -s ouroffice

This will drop you in the erlang shell. When the scanner spots
somebody coming online or offline, you'll see logs messages appear in
your console, and more when it has posted to Twitter.
