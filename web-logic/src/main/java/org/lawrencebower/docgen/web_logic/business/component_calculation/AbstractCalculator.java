package org.lawrencebower.docgen.web_logic.business.component_calculation;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.utils.ViewUtils;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_logic.view.document_info.component.DocComponentView;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

public abstract class AbstractCalculator {

    @Autowired
    ViewUtils viewUtils;

    protected Float runCalculation(List<Float> operandValues, Operator operator) {
        checkOperandSize(operandValues);
        float result = operandValues.get(0);
        for (int i = 1; i < operandValues.size(); i++) {
            Float rhs = operandValues.get(i);
            result = Operator.applyOperation(operator, result, rhs);
        }

        return result;
    }

    protected void checkOperandSize(List<Float> operandValues) {
        if ((operandValues == null) || operandValues.isEmpty()) {
            throw new DocGenException("List of operands is empty!?");
        }
    }

    protected void runCalculationOnOperandsIfNeeded(List<String> operands,
                                                    List<DocumentInfoView> allDocs) {
        for (String operand : operands) {
            runAllOperandCalculationsIfNeeded(allDocs, operand);
        }
    }

    private void runAllOperandCalculationsIfNeeded(List<DocumentInfoView> allDocs, String operand) {

        List<DocComponentView> components = viewUtils.getAllDocComponentViewsWithName(operand, allDocs);

        for (DocComponentView component : components) {
            if (component.hasCalculation()) {
                component.calculateValueIfNeeded(allDocs);
            }
        }
    }
}