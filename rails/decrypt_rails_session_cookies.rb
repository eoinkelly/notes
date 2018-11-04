##
# Notes
#
# * This is a modifed version of https://stackoverflow.com/questions/41474176/how-to-decrypt-a-rails-5-session-cookie-manually
# * The Rails 5.2 version does not work in development mode but works fine in production modes
#
# The decrypted session cookie looks like this for most Rails apps:
#
#     {
#       "session_id"=>"somestring",
#       "_csrf_token"=>"some_base64_encoded_string=="
#     }
#

# QUESTION: what does rails 5.2 do differently in development?
# QUESTION: does rails 5.1 do diff thing in prod and development?
# TODO: make a raw version of the rails 5.2 function

require "cgi"
require "active_support"
require "active_support/all"
require "openssl"

# verified in rails 5.2 production env
def rails_5_2_verify_and_decrypt_session_cookie(cookie, secret_key_base)
  cookie = CGI.unescape(cookie)
  salt   = "authenticated encrypted cookie"
  encrypted_cookie_cipher = "aes-256-gcm"
  serializer = ActiveSupport::MessageEncryptor::NullSerializer

  key_generator = ActiveSupport::KeyGenerator.new(secret_key_base, iterations: 1000)
  key_len = ActiveSupport::MessageEncryptor.key_len(encrypted_cookie_cipher)
  secret = key_generator.generate_key(salt, key_len)
  encryptor = ActiveSupport::MessageEncryptor.new(secret, cipher: encrypted_cookie_cipher, serializer: serializer)

  encryptor.decrypt_and_verify(cookie)
end

# verified this works for Rails 5.1
def rails_5_1_verify_and_decrypt_session_cookie(cookie, secret_key_base)
  cookie = CGI.unescape(cookie)
  salt         = "encrypted cookie"
  signed_salt  = "signed encrypted cookie"
  key_generator = ActiveSupport::KeyGenerator.new(secret_key_base, iterations: 1000)
  secret = key_generator.generate_key(salt)[0, ActiveSupport::MessageEncryptor.key_len]
  sign_secret = key_generator.generate_key(signed_salt)
  encryptor = ActiveSupport::MessageEncryptor.new(secret, sign_secret, serializer: JSON)

  encryptor.decrypt_and_verify(cookie)
end

# verified this works for Rails 5.1
def raw_rails_5_1_verify_and_decrypt_session_cookie(cookie, secret_key_base)
  cookie = CGI.unescape(cookie)

  #################
  # generate keys #
  #################
  # rails 5.1
  encrypted_cookie_salt = "encrypted cookie" # default: Rails.application.config.action_dispatch.encrypted_cookie_salt
  encrypted_signed_cookie_salt = "signed encrypted cookie" # default: Rails.application.config.action_dispatch.encrypted_signed_cookie_salt

  iterations = 1000
  key_size = 64
  secret = OpenSSL::PKCS5.pbkdf2_hmac_sha1(secret_key_base, encrypted_cookie_salt, iterations, key_size)[0, OpenSSL::Cipher.new("aes-256-cbc").key_len]
  sign_secret = OpenSSL::PKCS5.pbkdf2_hmac_sha1(secret_key_base, encrypted_signed_cookie_salt, iterations, key_size)

  ##########
  # Verify #
  ##########
  data, digest = cookie.split("--")
  fail "invalid message" unless digest == OpenSSL::HMAC.hexdigest(OpenSSL::Digest::SHA1.new, sign_secret, data)
  # you better use secure compare instead of `==` to prevent time based attact,
  # ref: ActiveSupport::SecurityUtils.secure_compare

  ###########
  # Decrypt #
  ###########
  encrypted_message = Base64.strict_decode64(data)
  encrypted_data, iv = encrypted_message.split("--").map{ |v| Base64.strict_decode64(v) }
  cipher = OpenSSL::Cipher::Cipher.new("aes-256-cbc")
  cipher.decrypt
  cipher.key = secret
  cipher.iv  = iv
  decrypted_data = cipher.update(encrypted_data)
  decrypted_data << cipher.final

  JSON.parse(decrypted_data)
end

# rubocop:disable Metrics/LineLength

##
# sample rails 5.1 app (development secret_key_base)
#
sample_session_cookie = "a0ZwenJDZkNWVzMrYmVmZG1vWnhUb0ZBVnEyZzcwQWFMRUF2NkFDK0lydWdzZFFFZ0JDQ1AxMFlvWXVXbldMVzk3Z0ZoemNLVTFaMWpubVJDSlhpMnk2WW9kQnU0V2FpMmZyakJrZ3hzVFZyOWJDNXNVdDRwQWJBYUxIb0lONnhpNXArNmd0Y1pzWDV5dDNBNE9Fd3NnPT0tLW9rNjBRZ2NjRVR4NGk1NEVZTHNCTVE9PQ%3D%3D--3295e895fa63424c9b49aa8eeab9b45d6d14e3bd"
secret_key_base_from_app = "4485608ec4bf6fced9f710fc1d8b7c740d1c4646e37b7e87af31bbd316c33d1e6a9529f295ce5111846c19f8029ac81bbe4281ac3846d84d30fad5488b716c38"
p raw_rails_5_1_verify_and_decrypt_session_cookie(sample_session_cookie, secret_key_base_from_app)
#
# Output:
#
# decrypt-session-cookies.rb:61: warning: constant OpenSSL::Cipher::Cipher is deprecated
# {"session_id"=>"b8a2ba4a6d4ada7e03f28b676e015381", "_csrf_token"=>"UUkRNMOeMxmhymTkVfLVXAQBrbgT0gLAQJJDhryPklA="}

##
# sample rails 5.2 app (secret_key_base)
# NOTE: this doesn't seem to work when you run the app in development mode - rails must do something different in that case
#
# sample_session_cookie = "55CM84WYhchgOLD%2FP3b5Uu8If2hRafgh3OVD1LukNkVdiS5rvtTi23%2FHjUX1Lhi%2FlAUsJq3d8XS2v2L32astrUaGQ32f86hQYY1srUJnazEj1xSPKl5D%2BA4JRJPInIBIMiaT3FUfqdB%2FiBO%2B4YY%3D--PpFnbKof1i0tvBzt--CJvBvJnqMLgKahUzM71gqw%3D%3D"
sample_session_cookie = "JIadS9AV03nzkiI47W1tG81WOlMzp5Rou4pFnwk6J8d2UIL3rMLvc0RAaL5AiY2fnNrBC0ftOWFGxxG4EXA21nqa4TyuS%2B5HubSx0A8Qmr2MA2dGN7w35XSXf0sHB6SNeTom%2Fzr5mpp55x65%2FUE%3D--wjPmm7XwBCLZgMSk--My7dw341mHUGOszisJWjEQ%3D%3D"
secret_key_base_from_app = "994ad389ebb3f610a8bb8d618b6f84a661a36add1fdf4d3df1ba9a2eecdace5d0d8afa4208a5df2713dd7a22b738e51ccda5bcce3e0a2e666473853cf201889c"

p rails_5_2_verify_and_decrypt_session_cookie(sample_session_cookie, secret_key_base_from_app)
# Output
#
# "{\"session_id\":\"df4a7b5c393a87c6bd32960948e33d3c\",\"_csrf_token\":\"M2F4TJw07iEWdNzlV8gNv7cyxK8S/TvDUPBY3lVjbDM=\"}"
#

# rubocop:enable Metrics/LineLength
