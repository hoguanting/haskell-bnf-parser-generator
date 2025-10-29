export type State = Readonly<{
    grammar: string;
    string: string;
    selectedParser: string;
    run: boolean;
    currentAction: "parse" | "save"; // indicates which action to send to the backend
}>;

export type Output = Readonly<{
    grammarParseError: string;
    parsers: readonly string[];
    parserOutput: string;
    warnings: readonly string[];
}>;
