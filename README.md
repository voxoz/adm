ADM: Monitoring Synrc Apps
==========================

Features
--------

Monitor KVS, N2O, BPE and other Synrc Applications.

Run
---

To try ADM you need to clone a ADM repo from Github and build it.
We use a very small and powerful tool called `mad`:

    $ npm install -g uglify
    $ git clone git://github.com/synrc/adm && cd adm
    $ ./mad dep com pla
    $ ./mad static min
    $ ./mad rep

Now you can try it out: [http://127.0.0.1:8108](http://127.0.0.1:8108)

Credits
-------

* Maxim Sokhatsky
* Arseniy Bushyn

OM A HUM
