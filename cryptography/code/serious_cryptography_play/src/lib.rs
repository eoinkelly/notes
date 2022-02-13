pub mod caesar {
    use std::collections::HashMap;

    /// Encrypt using Caesar algorithm with the default rotation of 3.
    ///
    /// # Examples
    ///
    /// ```
    /// # use serious_cryptography_play::caesar;
    /// let input = "caesar";
    /// let expected_output = "fdhvdu";
    /// assert_eq!(caesar::encrypt(input).unwrap(), expected_output);
    /// ```
    ///
    pub fn encrypt(plain_text: &str) -> Result<String, &str> {
        encrypt_w_rotation(plain_text, 3)
    }

    /// Encrypt using Caesar algorithm with the default rotation of 3.
    ///
    /// # Examples
    ///
    /// ```
    /// # use serious_cryptography_play::caesar;
    /// let input = "fdhvdu";
    /// let expected_output = "caesar";
    /// assert_eq!(caesar::decrypt(input).unwrap(), expected_output);
    /// ```
    ///
    pub fn decrypt(plain_text: &str) -> Result<String, &str> {
        decrypt_w_rotation(plain_text, 3)
    }

    /// Encrypt using Caesar algorithm with the given rotation.
    ///
    /// # Errors
    ///
    /// The `plain_text` parameter must be in ASCII range a-z. This function
    /// will not coerce the input but will return an error if it is not.
    ///
    /// # Examples
    ///
    /// ```
    /// # use serious_cryptography_play::caesar;
    /// let input = "caesar";
    /// let expected_output = "fdhvdu";
    /// assert_eq!(caesar::encrypt_w_rotation(input, 3).unwrap(), expected_output);
    /// ```
    //
    pub fn encrypt_w_rotation(plain_text: &str, rotation: usize) -> Result<String, &str> {
        // Exit early if the input is not ASCII alphabetic
        if !plain_text.chars().all(|x| x.is_ascii_lowercase()) {
            return Err("Input must be ASCII a-z");
        }

        let encryption_map = build_map(Purpose::Encryption, rotation);

        let result: String = plain_text
            .chars()
            .map(|p| *encryption_map.get(&p).unwrap())
            .collect();
        dbg!(&result);

        Ok(result)
    }

    /// Decrypt using Caesar algorithm with the given rotation.
    ///
    /// # Errors
    ///
    /// The `cipher_text` parameter must be in ASCII range a-z. This function
    /// will not coerce the input but will return an error if it is not.
    ///
    /// # Examples
    ///
    /// ```
    /// # use serious_cryptography_play::caesar;
    /// let expected_output = "caesar";
    /// let input = "fdhvdu";
    /// assert_eq!(caesar::decrypt_w_rotation(input, 3).unwrap(), expected_output);
    /// ```
    //
    pub fn decrypt_w_rotation(plain_text: &str, rotation: usize) -> Result<String, &str> {
        // Exit early if the input is not ASCII alphabetic
        if !plain_text.chars().all(|x| x.is_ascii_lowercase()) {
            return Err("Input must be ASCII a-z");
        }

        let decryption_map = build_map(Purpose::Decryption, rotation);

        let result: String = plain_text
            .chars()
            .map(|p| *decryption_map.get(&p).unwrap())
            .collect();
        dbg!(&result);

        Ok(result)
    }

    enum Purpose {
        Encryption,
        Decryption,
    }

    fn build_map(purpose: Purpose, rotation: usize) -> HashMap<char, char> {
        let plain_alphabet = 'a'..='z';
        let mut cipher_alphabet: Vec<char> = ('a'..='z').collect();

        match purpose {
            Purpose::Encryption => cipher_alphabet.rotate_left(rotation),
            Purpose::Decryption => cipher_alphabet.rotate_right(rotation),
        }

        plain_alphabet
            .into_iter()
            .zip(cipher_alphabet.into_iter())
            .collect()
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn caesar_encrypt_bad_input_numeric() {
            let input = "caesar1";
            let expected = Err("Input must be ASCII a-z");

            assert_eq!(encrypt_w_rotation(input, 3), expected);
        }

        #[test]
        fn caesar_encrypt_bad_input_uppercase() {
            let input = "Caesar";
            let expected = Err("Input must be ASCII a-z");

            assert_eq!(encrypt_w_rotation(input, 3), expected);
        }

        #[test]
        fn caesar_encrypt_works() {
            let input = "caesar";
            let expected = "fdhvdu";

            assert_eq!(encrypt_w_rotation(input, 3).unwrap(), expected);
        }
    }
}
