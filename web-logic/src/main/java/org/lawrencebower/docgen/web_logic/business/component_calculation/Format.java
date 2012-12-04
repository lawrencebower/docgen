package org.lawrencebower.docgen.web_logic.business.component_calculation;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.text.NumberFormat;

public enum Format {
    CURRENCY,
    NONE;

    public static String applyFormat(Format operator, Float operand) {

        switch (operator) {
            case CURRENCY:
                NumberFormat format = NumberFormat.getInstance();
                format.setMaximumFractionDigits(2);
                format.setMinimumFractionDigits(2);
                return format.format(operand);
            case NONE:
                return operand.toString();
        }

        String message = String.format("Format '%s' not recognized", operator);
        throw new DocGenException(message);
    }
}
