## Compilation & Loading

To compile and launch Erlang/OTP (with the required additional arguments):

```
make load
```

## Instrumentation

tracer:inst(tcp,[{dir,"examples"}]).

generates the instrumented source of examples/tcp.erl in the current directory

## Tracing

tracer:trace("tcp:main()",Opts).

does the tracing of function tcp:main().
It assumes to have the compiled version of the instrumented code in
the current directory.

Opts contains options specification of the form {optionName,value}. If
unspecified defaults are used.

log_dir: directory to be created for logs [default trace]
timeout: timeout for execution [default 10000]
