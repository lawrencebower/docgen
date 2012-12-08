package org.lawrencebower.docgen.web_logic.business.component_calculation;

import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;
import org.lawrencebower.docgen.web_logic.view.document.component.DocComponentView;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ComponentCalculation {

    private Operator operator;
    private Format format;
    private List<String> fields;
    private Float result;
    private boolean isRun;

    public static final float NO_RESULT = Float.MAX_VALUE;

    public ComponentCalculation(Operator operator,
                                String... fields){
        this(operator,
             Format.NONE,
             fields);
    }

    public ComponentCalculation(Operator operator,
                                Format format,
                                String... fields) {
        this.operator = operator;
        this.format = format;
        List<String> fieldList = Arrays.asList(fields);
        this.fields = Collections.unmodifiableList(fieldList);
    }

    public List<String> getOperands() {
        return fields;
    }

    public boolean isNotRun() {
        return !isRun();
    }

    public boolean isRun() {
        return isRun;
    }

    public void clearResult() {
        result = NO_RESULT;
    }

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

    public Float getResult() {
        return result;
    }

    public String getFormattedResult(){
        return Format.applyFormat(format, result);
    }

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
