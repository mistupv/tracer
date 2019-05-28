erl -pa ebin -eval "tracer:inst($1,[{dir,\"examples\"}]), halt()"  
erlc $1.erl
erl -pa ebin -eval "tracer:trace(\"$1:main()\",[{timeout,2000}])"
