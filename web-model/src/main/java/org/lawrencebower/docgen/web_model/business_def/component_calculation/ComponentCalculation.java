package org.lawrencebower.docgen.web_model.business_def.component_calculation;

import org.lawrencebower.docgen.web_model.view.document.DocumentSet;

import java.math.BigDecimal;
import java.util.List;

public interface ComponentCalculation {

    BigDecimal NO_RESULT = BigDecimal.ZERO;

    List<String> getOperands();

    boolean isNotRun();

    boolean isRun();

    void clearResult();

    void runOnOperand(String operandValue);

    BigDecimal getResult();

    String getFormattedResult();

    void runOnOperands(DocumentSet documentSet);
}
