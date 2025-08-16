Example of `auth`

```
pry(1)> auth
%Ueberauth.Auth{
  credentials: %Ueberauth.Auth.Credentials{
    expires: true,
    expires_at: 1536003087,
    other: %{},
    refresh_token: nil,
    scopes: ["https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/plus.me"],
    secret: nil,
    token: "ya29.GlwNBnrCiLR2xbD0bum3BTd ...",
    token_type: "Bearer"
  },
  extra: %Ueberauth.Auth.Extra{
    raw_info: %{
      token: %OAuth2.AccessToken{
        access_token: "ya29.GlwNBnrCiLR2xbD0bum3BTd ...",
        expires_at: 1536003087,
        other_params: %{
          "id_token" => "eyJhbGciOiJSUzI1NiIsImtpZCI6IjU1Yjg1NGVkZjM1 ...",
          "scope" => "https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/plus.me"
        },
        refresh_token: nil,
        token_type: "Bearer"
      },
      user: %{
        "email" => "me@gmail.com",
        "email_verified" => true,
        "family_name" => "Kelly",
        "gender" => "male",
        "given_name" => "Eoin",
        "name" => "Eoin Kelly",
        "picture" => "https://lh6.googleusercontent.com/... aHeuiXYW-c/photo.jpg",
        "profile" => "https://plus.google.com/1111",
        "sub" => "111 ..."
      }
    }
  },
  info: %Ueberauth.Auth.Info{
    description: nil,
    email: "me@gmail.com",
    first_name: "Eoin",
    image: "https://lh6.googleusercontent.com/ ... euiXYW-c/photo.jpg",
    last_name: "Kelly",
    location: nil,
    name: "Eoin Kelly",
    nickname: nil,
    phone: nil,
    urls: %{
      profile: "https://plus.google.com/1111",
      website: nil
    }
  },
  provider: :google,
  strategy: Ueberauth.Strategy.Google,
  uid: "1111 ..."
}
```
