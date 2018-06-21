make
erl -sname main -setcookie cookie -pa ebin
# tracer:trace("acknowledge:main()", self(), [{dir,"examples"},{log_dir,"inst"}]).
# wrapper:trace("acknowledge:main()", self(), [{dir,"examples"},{log_dir,"norm"}]).