import { State } from "../State";

/**
  * Parse a block with expressions. Expressions can be separated by a newline
  * character '\n', or by a semicolon ';'. In case of a semicolon, no output
  * of the preceding line is returned.
  * @return {Node} node
  * @private
  */
export function parseBlock(state: State) {
    let node;
    const blocks = [];
    let visible;

    const token = state.tokenValue;

    if (token !== '' && token !== '\n' && token !== ';') {
        node = this.parseAssignment()
        if (this.state.comment) {
            node.comment = this.state.comment
        }
    }

    // TODO: simplify this loop
    while (token === '\n' || token === ';') { // eslint-disable-line no-unmodified-loop-condition
        if (blocks.length === 0 && node) {
            visible = (!state.token.equal(';'))
            blocks.push({ node, visible })
        }

        const token = state.getToken();
        if (token !== '\n' && token !== ';' && token !== '') {
            node = this.parseAssignment()
            if (this.state.comment) {
                node.comment = this.state.comment
            }

            visible = (token !== ';')
            blocks.push({ node, visible })
        }
    }

    if (blocks.length > 0) {
        return new BlockNode(blocks)
    } else {
        if (!node) {
            node = new ConstantNode(undefined)
            if (this.state.comment) {
                node.comment = this.state.comment
            }
        }

        return node
    }
}