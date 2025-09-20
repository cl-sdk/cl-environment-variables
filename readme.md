# cl-environment-variables

`cl-environment-variables` is a Common Lisp library for **declaring, validating, and accessing environment variables**.

Applications often rely on environment variables for configuration (database credentials, API keys, feature flags, etc.). Without validation, missing or malformed variables can lead to runtime errors. This library provides a **declarative interface** to define configuration requirements and ensures they are **validated automatically at compile-time and load-time**.

---

## Features

* **Declarative definitions**
  Specify expected environment variables with types and defaults. All variables are required by default, or you must specify a default value.

* **Early validation**
  Variables are checked at compile/load time to fail fast if something is missing.

* **Typed access**
  Automatically converts values into Lisp types (`string`, `integer`, `boolean`, etc.).

* **Extensible**
  Custom validators can be defined for domain-specific types.

* **Simple API**
  Minimal interface designed for application configuration.

---

## Example Usage

```lisp
(cl-environment-variables:define-env-vars
  (:db-host :string)
  (:db-port :integer :default 5432)
  (:debug-mode :boolean :default nil))

;; Validation happens automatically at compile/load time.
;; If DB_HOST is missing, loading fails with an error.

(cl-environment-variables:environment-variable :db-host)    ;; => "localhost"
(cl-environment-variables:environment-variable :db-port)    ;; => 5432
(cl-environment-variables:environment-variable :debug-mode) ;; => NIL
```

---

## How It Works

* Use `DEFINE-ENV-VARS` to declare environment variables.
* Required variables that are missing signal errors immediately.
* Values are cached and accessible through `GET-ENV`.

---

## License

This project is licensed under [The Unlicense](http://unlicense.org/).
It is released into the public domain — you are free to use, modify, and distribute it for any purpose.
