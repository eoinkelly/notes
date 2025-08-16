# CSV Injection

- https://owasp.org/www-community/attacks/CSV_Injection
- examples
    - https://medium.com/cryptogennepal/what-is-a-csv-injection-attack-9208b54b4598
- site generates CSV file and includes untrusted user input within it
- also known as formula injections
- uses
    - data exfiltration
    - code execution on the machine of the user who downloads the file (with the
      permissions of that user)
- csv_safe gem is very simple
  https://github.com/zvory/csv-safe/blob/master/lib/csv-safe.rb
