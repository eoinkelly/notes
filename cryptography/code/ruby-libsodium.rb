require "rbnacl"

# https://github.com/RubyCrypto/rbnacl/wiki/SimpleBox

key = RbNaCl::Random.random_bytes(RbNaCl::SecretBox.key_bytes)
box = RbNaCl::SimpleBox.from_secret_key(key)

plaintext = <<~EOM
	This is my important secret
EOM

p ciphertext = box.encrypt(plaintext)

p new_plaintext = box.decrypt(ciphertext)