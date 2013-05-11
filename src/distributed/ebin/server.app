{application, server,
 [{vsn, "1.0.0"},
  {description, "Erlang MUD server"},
  {modules, [server]},
  {applications, [server, stdlib, kernel]},
  {registered, [server]}
 ]}.