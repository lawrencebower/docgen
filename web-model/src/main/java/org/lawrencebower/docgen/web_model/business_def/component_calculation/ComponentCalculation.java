package org.lawrencebower.docgen.web_model.business_def.component_calculation;

import org.lawrencebower.docgen.web_model.view.document.DocumentSet;

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
