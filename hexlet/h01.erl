-module(h01).

-compile(export_all).

-record(user, {id :: integer(),
							name :: string(),
							age :: integer(),
							country :: string(),
							city :: string()
						}).