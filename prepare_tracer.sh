make
cp ebin/* examples/
cd examples
erl -sname edd_main -setcookie edd_cookie
# put(modules_to_instrument,[]),edd_trace:trace("client_server:main()",10000,self(),".").
# edd_trace:trace("client_server:main()", self()).