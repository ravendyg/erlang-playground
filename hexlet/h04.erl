-module(h04).

-compile(export_all).

do_smth([]) -> first;
do_smth([{X, Y} | _]) -> {X,Y};
do_smth([Head | _]) -> Head.