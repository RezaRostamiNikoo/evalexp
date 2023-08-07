import * as ts from "typescript";


export function creating() {
    const file = ts.createSourceFile("source.ts", "", ts.ScriptTarget.ESNext, false, ts.ScriptKind.TS);
    const printer = ts.createPrinter({ newLine: ts.NewLineKind.LineFeed });

    const dictValueIdentifier = ts.factory.createIdentifier("T");

    const key = ts.factory.createParameterDeclaration(
        undefined,
        undefined,
        "key",
        undefined,
        ts.factory.createTypeReferenceNode("string")
    );

    const index = ts.factory.createIndexSignature(
        undefined,
        [key],
        ts.factory.createTypeReferenceNode(dictValueIdentifier)
    );

    const dictDecl = ts.factory.createInterfaceDeclaration(
        undefined,
        "Dict",
        [ts.factory.createTypeParameterDeclaration(undefined, dictValueIdentifier)],
        undefined,
        [index]
    );

    const result = printer.printNode(ts.EmitHint.Unspecified, dictDecl, file);
    console.log(result);
}