package org.lawrencebower.docgen.web_logic.business.component_calculation;

import org.lawrencebower.docgen.core.exception.DocGenException;

public enum Operator {
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE;

    public static Float applyOperation(Operator operator, Float lhs, Float rhs) {
        switch (operator) {
            case PLUS:
                return lhs + rhs;
            case MINUS:
                return lhs - rhs;
            case MULTIPLY:
                return lhs * rhs;
            case DIVIDE:
                return lhs / rhs;
        }

        String message = String.format("Operator '%s' not recognized", operator);
        throw new DocGenException(message);
    }
}
