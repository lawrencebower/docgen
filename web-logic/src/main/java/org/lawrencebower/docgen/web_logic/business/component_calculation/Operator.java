package org.lawrencebower.docgen.web_logic.business.component_calculation;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.math.BigDecimal;
import java.math.RoundingMode;

public enum Operator {
    PLUS,
    MINUS,
    MULTIPLY,
    DIVIDE;

    public static BigDecimal applyOperation(Operator operator, BigDecimal lhs, BigDecimal rhs) {
        switch (operator) {
            case PLUS:
                return lhs.add(rhs);
            case MINUS:
                return lhs.subtract(rhs);
            case MULTIPLY:
                BigDecimal multiply = lhs.multiply(rhs);
                return roundResult(multiply);
            case DIVIDE:
                BigDecimal divide = lhs.divide(rhs);
                return roundResult(divide);
        }

        String message = String.format("Operator '%s' not recognized", operator);
        throw new DocGenException(message);
    }

    /**
     * round results to be suitable for money
     */
    private static BigDecimal roundResult(BigDecimal result) {
        return result.setScale(2, RoundingMode.HALF_EVEN);
    }
}
