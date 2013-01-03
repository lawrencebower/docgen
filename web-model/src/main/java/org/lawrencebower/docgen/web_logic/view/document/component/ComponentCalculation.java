package org.lawrencebower.docgen.web_logic.view.document.component;

import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;

import java.util.List;

public interface ComponentCalculation {

    float NO_RESULT = Float.MAX_VALUE;

    List<String> getOperands();

    boolean isNotRun();

    boolean isRun();

    void clearResult();

    void runOnOperand(Float operandValue);

    Float getResult();

    String getFormattedResult();

    void runOnOperands(DocumentSet documentSet);
}
