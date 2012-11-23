package org.lawrencebower.docgen.web_logic.business.component_calculation.text;

import org.lawrencebower.docgen.web_logic.business.component_calculation.AbstractCalculator;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.business.component_calculation.Operator;
import org.lawrencebower.docgen.web_logic.business.utils.ViewUtils;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.TextComponentView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;

public class BasicTextComponentCalculator extends AbstractCalculator implements TextComponentCalculator {

    @Autowired
    ViewUtils viewUtils;

    @Override
    public void runCalculation(TextComponentView textComponentView,
                               ComponentCalculation calculation,
                               List<DocumentInfoView> allDocs) {

        List<String> operands = calculation.getOperands();

        List<Float> operandValues = new ArrayList<>();

        runCalculationOnOperandsIfNeeded(operands, allDocs);

        for (String operand : operands) {
            List<Float> values = viewUtils.getOperandValuesByName(operand, allDocs);
            operandValues.addAll(values);
        }

        Operator operator = calculation.getOperator();
        Float calculationResult = runCalculation(operandValues, operator);

        textComponentView.setComponentValue(calculationResult);
    }

}
