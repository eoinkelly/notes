# CSRF

    https://www.owasp.org/index.php/Cross-Site_Request_Forgery_(CSRF)

- It is where an authenticated user is tricked into taking action on your
  site/API
- The user is already authenticated with you!
- As server devs we are just trying to help the browser prevent this trickery
- A CSRF attack can be carried out with **any** HTTP method
    - GET
        - `GET http://app.com/users/1/delete`
    - POST
        - create a form which submits to http://app.com/users/1/delete.
        - make the form look like some other form or make it submit
          automatically `<body onload="document.forms[0].submit()">`
    - PUT/PATCH/DELETE etc.
        ```
        <script>
        function put() {
            var x = new XMLHttpRequest()
            x.open("PUT", "http://app.com/users/1/delete");
            x.setRequestHeader("Content-Type", "application/json");
            x.send(JSON.stringify({}));
            console.log("Ouch");
        }
        </script>
        <body onload="put()">
        ...
        ```

        - Note the browser will block this JS based version because of its same
          origin policy
            - YOu can break this protection by setting
              `Access-Control-Allow-Origin: *` HTTP header on your server. Don't
              do this!

Rails CSRF mitigation puts a secret in a META header of the site and expects
that secret back as a HTTP header when you submit a non GET request.

TOOD: Revise how cookies work TODO: Revise same-origin policy QUESTION: Why not
care about GET requests QUESTION: why can the attacker not see data?
