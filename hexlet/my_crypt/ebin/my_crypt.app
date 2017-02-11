{application, my_crypt, [
    {description, "Crypto app"},
    {vsn, "0.1"},
    {modules, [my_crypt, my_crypt_sup, my_crypt_worker]},
    {registered, []},
    {applications, [
        kernel,
        stdlib
    ]},
    {mod, {my_crypt, []}},
    {env, [
    ]}
]}.