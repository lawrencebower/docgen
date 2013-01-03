package org.lawrencebower.docgen.web_logic.business.component_calculation;

import org.lawrencebower.docgen.web_model.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.component.DocComponentView;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ComponentCalculationImpl implements ComponentCalculation {

    private Operator operator;
    private Format format;
    private List<String> fields;
    private Float result;
    private boolean isRun;

    public ComponentCalculationImpl(Operator operator,
                                    String... fields){
        this(operator,
             Format.NONE,
             fields);
    }

    public ComponentCalculationImpl(Operator operator,
                                    Format format,
                                    String... fields) {
        this.operator = operator;
        this.format = format;
        List<String> fieldList = Arrays.asList(fields);
        this.fields = Collections.unmodifiableList(fieldList);
    }

    @Override
    public List<String> getOperands() {
        return fields;
    }

    @Override
    public boolean isNotRun() {
        return !isRun();
    }

    @Override
    public boolean isRun() {
        return isRun;
    }

    @Override
    public void clearResult() {
        result = NO_RESULT;
    }

    @Override
    public void runOnOperand(Float operandValue) {
        /**
         * If this is the first term in the calculation, set the operand as the result
         */
        if(result == NO_RESULT){
            result = operandValue;
        }else{
            result = Operator.applyOperation(operator, result, operandValue);
        }
    }

    @Override
    public Float getResult() {
        return result;
    }

    @Override
    public String getFormattedResult(){
        return Format.applyFormat(format, result);
    }

    @Override
    public void runOnOperands(DocumentSet documentSet) {

        List<DocComponentView> componentViews = documentSet.getAllComponentViewsFromDocs();

        List<String> operands = getOperands();
        for (String operand : operands) {
            runCalculationOnOperand(operand,
                                    componentViews,
                                    documentSet);
        }

        isRun = true;
    }

    private void runCalculationOnOperand(String operand,
                                         List<DocComponentView> componentViews,
                                         DocumentSet documentSet) {

        for (DocComponentView componentView : componentViews) {
            boolean run = componentView.runCalculationIfMatch(operand,
                                                              this,
                                                              documentSet);
            if(run){
                break;//only run the calculation on the first component to match
            }
        }
    }

}
