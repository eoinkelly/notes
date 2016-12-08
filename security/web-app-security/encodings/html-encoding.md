# Encoding characters in HTML documents

The default character set for HTML5 is UTF-8

Characters can be added to an HTML document in 4 ways:

1. typing the character directly (only works for printable characters)
2. entity name (not all characters have entity names)
3. entity decimal value
4. entity hex value

    <p>My name is Johnny "Bang" Johnson</p>
    <p>My name is Johnny &quot;Bang&quot; Johnson</p>
    <p>My name is Johnny &#34;Bang&#34; Johnson</p>
    <p>My name is Johnny &#x0022;Bang&#x0022; Johnson</p>

So there are 4 ways to add any given character to a HTML document
