package org.lawrencebower.docgen.web_logic.business.component_calculation;

import org.lawrencebower.docgen.core.exception.DocGenException;

import java.text.NumberFormat;

public enum Format {
    CURRENCY,
    NONE;

    public static String applyFormat(Format operator, Float operand) {

        String returnVal = null;

        switch (operator) {
            case CURRENCY:
                NumberFormat format = NumberFormat.getInstance();
                format.setMaximumFractionDigits(2);
                format.setMinimumFractionDigits(2);
                format.setGroupingUsed(false);
                returnVal = format.format(operand);
                break;
            case NONE:
                returnVal = operand.toString();
                break;
            default:
                String message = String.format("Format '%s' not recognized", operator);
                throw new DocGenException(message);
        }

        return returnVal;
    }
}
