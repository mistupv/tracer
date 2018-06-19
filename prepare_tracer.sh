make
erl -sname edd_main -setcookie edd_cookie -pa ebin
# put(modules_to_instrument,[]),edd_trace:trace("client_server:main()",10000,self(),".").
# edd_trace:trace("client_server:main()", self(), [{dir, "examples"}]).