# BNF2Haskell: A BNF Parser Generator

This project is a parser generator built in Haskell. It takes a formal grammar specified in **Backus-Naur Form (BNF)** as input and generates a complete, functional **Haskell parser module** as output. This generated module can then be used to parse text that conforms to the input grammar.

The project includes two main components:

1.  A core test suite that validates the parser generation against example files.
2.  A full-stack interactive web application (Haskell backend, JavaScript frontend) that provides a live editor to test the generator.

---

## 🌟 Features

This generator includes a robust interactive web interface with several key features:

* **Save Generated Code:** Users can save the generated Haskell parser module directly to a `.hs` file using the "Save Haskell" button.

* **Live Grammar Validation:** The system provides real-time feedback in a "Validation" panel, detecting common grammar errors, including:
    * Duplicated rules
    * Undefined non-terminals
    * Left-recursive rules
    * Incorrect arity (mismatched number of arguments)

* **Robust Parser Generation:** Any rules that are broken or invalid are automatically excluded from the generated Haskell code and the parser selection list, preventing compilation errors.

-----

> ### 📜 Course & Author Information
>
> This project was completed for **Assignment 2** of the **FIT2102 Programming Paradigms** course.
>
> **Original Author:** FIT2102 Programming Paradigms
>
> **Note from the course:** Please do not change the names of the functions defined in the `Assignment.hs` file. You may (and are highly encouraged) to implement your parsers **alongside** these pre-defined functions.

-----

## 🧪 Running the Core Test Suite

This is the primary method for validating the generator's correctness. The test script will automatically find all sample BNF files, run your generator on them, and save the results.

1.  Navigate to the project's root directory.

2.  Run the test suite using Stack:

    ```bash
    $ stack test
    ```

This command will:

  * Read the example grammar files from `examples/input/`.
  * Run your generator to produce Haskell parser code.
  * Save the generated `.hs` files to `examples/output/`.

You can then inspect the files in `examples/output/` to see the generated Haskell parsers.

-----

## 🖥️ Running the Interactive Web App

This project also includes an interactive web page to demonstrate the generator in real-time. It runs as a client-server application.

**You will need two separate terminals.**

### 1\. Start the Haskell Backend (Server)

In your **first terminal**, navigate to the `haskell` folder and run the server using Stack:

```bash
# Navigate to the Haskell backend directory
$ cd haskell

# Build and run the server
$ stack run
```

This will start the web server, which waits for API requests from the frontend.

### 2\. Start the JavaScript Frontend (Client)

In your **second terminal**, navigate to the `javascript` folder, install the dependencies, and run the development server:

```bash
# Navigate to the JavaScript frontend directory
$ cd javascript

# Install all required node modules
$ npm i

# Run the frontend development server
$ npm run dev
```

This will open a link in your browser (usually `http://localhost:5173` or similar). You can now type your own BNF grammar into the left-hand panel and see the generated Haskell parser code on the right.

<img width="1836" height="916" alt="image" src="https://github.com/user-attachments/assets/082eac4b-94e6-4d72-ba47-8664af21b030" />
