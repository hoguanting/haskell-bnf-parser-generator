import {
    combineLatest,
    debounceTime,
    fromEvent,
    map,
    merge,
    mergeWith,
    pairwise,
    scan,
    share,
    startWith,
    Subject,
    switchMap,
    type Observable,
} from "rxjs";
import { fromFetch } from "rxjs/fetch";
import type { State, Output } from "./types";

const grammarInput = document.getElementById("grammar-input") as HTMLTextAreaElement;
const stringInput = document.getElementById("string-input") as HTMLTextAreaElement;
const dropDown = document.getElementById("parserSelect") as HTMLSelectElement;
const button = document.getElementById("runParser")!;
const saveButton = document.getElementById("saveButton")!;

type Action<T> = (_: T) => T;

const resetState: Action<State> = s => ({
    ...s,
    run: false,
    currentAction: "parse",
});

// Create an Observable for keyboard input events
const input$: Observable<Action<State>> = fromEvent<KeyboardEvent>(
    grammarInput,
    "input",
).pipe(
    debounceTime(1000),
    map(event => (event.target as HTMLInputElement).value),
    map(value => s => resetState({ ...s, grammar: value })),
);

const stringToParse$: Observable<Action<State>> = fromEvent<KeyboardEvent>(
    stringInput,
    "input",
).pipe(
    debounceTime(1000),
    map(event => (event.target as HTMLInputElement).value),
    map(value => s => resetState({ ...s, string: value })),
);

const selectedParser$ = new Subject<string>();
const dropDownStream$: Observable<Action<State>> = fromEvent(dropDown, "change").pipe(
    map(event => (event.target as HTMLSelectElement).value),
    mergeWith(selectedParser$),
    map(
        (value): Action<State> =>
            s =>
                resetState({ ...s, selectedParser: value }),
    ),
);

const buttonStream$: Observable<Action<State>> = fromEvent(button, "click").pipe(
    map(
        (): Action<State> => {
            const latestGrammar = grammarInput.value;
            const latestString = stringInput.value;
            const latestParser = dropDown.value;
            return s => ({
                ...resetState(s),
                grammar: latestGrammar,
                string: latestString,
                selectedParser: latestParser || s.selectedParser,
                run: true,
                currentAction: "parse",
            });
        },
    ),
);

// Fires when "Save Haskell" is clicked; captures latest grammar/parser and marks action as "save".
const saveButtonStream$: Observable<Action<State>> = fromEvent(saveButton, "click").pipe(
    map(
        (): Action<State> => {
            const latestGrammar = grammarInput.value;
            const latestParser = dropDown.value;
            return s => ({
                ...resetState(s),
                grammar: latestGrammar,
                selectedParser: latestParser || s.selectedParser,
                currentAction: "save",
            });
        },
    ),
);

/**
 * Issue a POST to /api/generate and turn the JSON response into a function
 * that updates the Output portion of our state.
 */
function getOutput(state: State): Observable<Action<Output>> {
    // Get the HTML as a stream
    const body = new URLSearchParams();
    body.set("grammar", state.grammar);
    body.set(
        "string",
        state.currentAction === "parse" && state.run ? state.string : "",
    );
    body.set("selectedParser", state.selectedParser);
    body.set("action", state.currentAction);

    return fromFetch<
        Readonly<
            | { parsers: string; result: string; warnings: string }
            | { error: string }
        >
    >("/api/generate", {
        method: "POST",
        headers: {
            "Content-Type": "application/x-www-form-urlencoded",
        },
        body: body.toString(),
        selector: res => res.json(),
    }).pipe(
        map(res => {
            if ("error" in res) {
                return (output: Output) => ({
                    ...output,
                    grammarParseError: res.error,
                });
            }
            const parsers =
                res.parsers === ""
                    ? []
                    : res.parsers.split(",").filter(parser => parser.length > 0);
            const warnings =
                res.warnings === ""
                    ? []
                    : res.warnings.split("\n").filter(warning => warning.length > 0);
            return (output: Output) => ({
                ...output,
                grammarParseError: "",
                warnings,
                parserOutput: res.result,
                parsers,
            });
        }),
    );
}

const initialState: State = {
    grammar: "",
    string: "",
    selectedParser: "",
    run: false,
    currentAction: "parse",
};

const initialOutput: Output = {
    grammarParseError: "",
    parsers: [],
    parserOutput: "",
    warnings: [],
};

/**
 * Wire up the UI once the page loads: subscribe to changes, send requests,
 * and push the latest parser results/warnings into the DOM.
 */
function main() {
    const selectElement = document.getElementById("parserSelect")!;
    const grammarParseErrorOutput = document.getElementById(
        "grammar-parse-error-output",
    ) as HTMLOutputElement;
    const parserOutput = document.getElementById(
        "parser-output",
    ) as HTMLOutputElement;
    const validateOutput = document.getElementById(
        "validate-output",
    ) as HTMLOutputElement;

    // Subscribe to the input Observable to listen for changes
    const state$ = merge(
        input$,
        dropDownStream$,
        stringToParse$,
        buttonStream$,
        saveButtonStream$,
    ).pipe(
        scan((state, action) => action(state), initialState),
        share(),
    );

    const output$ = state$.pipe(
        switchMap(getOutput),
        scan((output, action) => action(output), initialOutput),
    );

    combineLatest([state$, output$])
        .pipe(
            map(([state, output]) => {
                const parsers = output.parsers;
                const selectedParser =
                    parsers.length === 0
                        ? ""
                        : parsers.includes(state.selectedParser)
                          ? state.selectedParser
                          : parsers[0];
                return {
                    ...state,
                    ...output,
                    selectedParser,
                };
            }),
            startWith({ ...initialState, ...initialOutput, selectedParser: "" }),
            pairwise(),
            map(([previous, current]) => ({
                ...current,
                resetParsers:
                    current.parsers.length !== previous.parsers.length ||
                    current.parsers.some(
                        (parser, index) => parser !== previous.parsers[index],
                    ),
            })),
        )
        .subscribe(state => {
            if (state.resetParsers) {
                selectElement.replaceChildren(
                    ...state.parsers.map(optionText => {
                        const option = document.createElement("option");
                        option.value = optionText;
                        option.text = optionText;
                        return option;
                    }),
                );
                // if the <option> HTML elements are changed, the value of the
                // <select> element will reset to ""
                dropDown.value = state.selectedParser;
                selectedParser$.next(state.selectedParser);
            }

            grammarParseErrorOutput.value = state.grammarParseError;
            parserOutput.value = state.parserOutput;
            validateOutput.value = state.warnings.join("\n");
        });
}
if (typeof window !== "undefined") {
    window.addEventListener("load", main);
}
