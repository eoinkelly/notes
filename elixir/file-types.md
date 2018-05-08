```
mix phx.new APPNAME --no-brunch
mix deps.get
mix deps.compile
cd APPNAME


# didn't create any thing in the repo
mix ecto.create
# created APPNAME_dev (but not test)


# run the server
mix phx.server
iex -S mix phx.server
```





Mix archives seem to contain two kinds of file: .beam, .app

The `.app` file is plain text and has the name of the command you run to run the archive
The beam files are compiled erlang modules

The package structure is

```
~/.mix/archives/PACKAGE_NAME-VERSION_NUMBER/ebin/*.app
~/.mix/archives/PACKAGE_NAME-VERSION_NUMBER/ebin/*.beam
```

```
*.beam      Erlang BEAM file
*.app       Plain text
```

```
{application,hex,
             [{description,"hex"},
              {modules,['Elixir.Hex','Elixir.Hex.API','Elixir.Hex.API.Auth',
                        'Elixir.Hex.API.Key','Elixir.Hex.API.Package',
                        'Elixir.Hex.API.Package.Owner',
                        'Elixir.Hex.API.Release','Elixir.Hex.API.ReleaseDocs',
                        'Elixir.Hex.API.User','Elixir.Hex.Config',
                        'Elixir.Hex.Crypto',
                        'Elixir.Hex.Crypto.AES_CBC_HMAC_SHA2',
                        'Elixir.Hex.Crypto.AES_GCM',
                        'Elixir.Hex.Crypto.ContentEncryptor',
                        'Elixir.Hex.Crypto.Encryption',
                        'Elixir.Hex.Crypto.KeyManager',
                        'Elixir.Hex.Crypto.PBES2_HMAC_SHA2',
                        'Elixir.Hex.Crypto.PKCS5',
                        'Elixir.Hex.Crypto.PublicKey','Elixir.Hex.HTTP',
                        'Elixir.Hex.HTTP.Certs','Elixir.Hex.HTTP.SSL',
                        'Elixir.Hex.HTTP.VerifyHostname','Elixir.Hex.Mix',
                        'Elixir.Hex.OptionParser','Elixir.Hex.Parallel',
                        'Elixir.Hex.Registry','Elixir.Hex.Registry.Server',
                        'Elixir.Hex.RemoteConverger','Elixir.Hex.Repo',
                        'Elixir.Hex.Resolver',
                        'Elixir.Hex.Resolver.Backtracks','Elixir.Hex.SCM',
                        'Elixir.Hex.Server','Elixir.Hex.Set',
                        'Elixir.Hex.Shell','Elixir.Hex.State',
                        'Elixir.Hex.Tar','Elixir.Hex.UpdateChecker',
                        'Elixir.Hex.Utils','Elixir.Hex.Version',
                        'Elixir.Hex.Version.InvalidRequirementError',
                        'Elixir.Hex.Version.InvalidVersionError',
                        'Elixir.Hex.Version.Requirement',
                        'Elixir.Mix.Tasks.Hex','Elixir.Mix.Tasks.Hex.Audit',
                        'Elixir.Mix.Tasks.Hex.Build',
                        'Elixir.Mix.Tasks.Hex.Config',
                        'Elixir.Mix.Tasks.Hex.Docs',
                        'Elixir.Mix.Tasks.Hex.Info',
                        'Elixir.Mix.Tasks.Hex.Install',
                        'Elixir.Mix.Tasks.Hex.Organization',
                        'Elixir.Mix.Tasks.Hex.Outdated',
                        'Elixir.Mix.Tasks.Hex.Owner',
                        'Elixir.Mix.Tasks.Hex.Publish',
                        'Elixir.Mix.Tasks.Hex.Repo',
                        'Elixir.Mix.Tasks.Hex.Retire',
                        'Elixir.Mix.Tasks.Hex.Search',
                        'Elixir.Mix.Tasks.Hex.User',hex_erl_tar,hex_filename,
                        hex_pb_package,hex_pb_signed,safe_erl_term]},
              {registered,[]},
              {vsn,"0.17.3"},
              {applications,[kernel,stdlib,elixir,ssl,inets]},
              {mod,{'Elixir.Hex',[]}}]}.
```

Breaking down the shape of `hex.app`:

```
{
    application,
    hex,
    [
        {
            description,
            "hex"
        },
        {
            modules,
            ['Elixir.Hex', ...otherpackagenames...  ]
        },
        {
            registered,
            []
        },
        {
            vsn,
            "0.17.3"
        },
        {
            applications,
            [kernel,stdlib,elixir,ssl,inets]
        },
        {
            mod,
            {
                'Elixir.Hex',[]
            }
        }
    ]
}.
```
