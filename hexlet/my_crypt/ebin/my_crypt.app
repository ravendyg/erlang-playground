{application, my_crypt, [
    {description, "Crypto app"},
    {vsn, "0.1"},
    {modules, [my_crypt]},
    {registered, []},
    {applications, [
        kernel,
        stdlib
    ]},
    {mod, {my_crypt, []}},
    {env, [
    ]}
]}.