package org.lawrencebower.docgen.web_logic.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoSet;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;

import java.util.List;

public class TextComponentView extends DocComponentView<TextComponent> {

    private ComponentCalculation componentCalculation;

    protected TextComponentView() {//force spring creation
        componentViewType = ComponentViewType.TEXT;
    }

    @Override
    public void setComponent(TextComponent docComponent) {
        super.setComponent(docComponent);
    }

    @Override
    public void setComponentValue(Boolean value) {
        String booleanString = value.toString();
        docComponent.setText(booleanString);
    }

    @Override
    public void setComponentValue(String value) {
        docComponent.setText(value);
    }

    @Override
    public void setComponentValue(Float value) {
        String floatString = value.toString();
        docComponent.setText(floatString);
    }

    @Override
    public void checkAndSetValueFromParamString(String componentName, String value) {
        String thisComponentName = getName();
        if (componentName.equals(thisComponentName)) {
            docComponent.setText(value);
        }
    }

    public String getStringValue() {
        return docComponent.getTextString();
    }

    @Override
    public Float getFloatValue() {
        String text = docComponent.getTextString();
        try {
            return Float.parseFloat(text);
        } catch (NumberFormatException e) {
            String messageTemplate = "Could not parse text value '%s' into a Float";
            String message = String.format(messageTemplate, text);
            throw new DocGenException(message);
        }
    }

    @Override
    public Boolean getBooleanValue() {
        String text = docComponent.getTextString();

        if (text.equals(Boolean.TRUE) || text.equals(Boolean.FALSE)) {
            return Boolean.parseBoolean(text);
        }

        String messageTemplate = "Could not parse text value '%s' into a Boolean";
        String message = String.format(messageTemplate, text);
        throw new DocGenException(message);
    }

    @Override
    public void injectProducts(List<ProductView> products) {
        //not implemented - just exit quietly
    }

    public boolean hasCalculation() {
        return componentCalculation != null;
    }

    @Override
    public void calculateValueIfNeeded(DocumentInfoSet documentSet) {
        if(hasCalculation() && componentCalculation.isNotRun()){
            componentCalculation.clearResult();
            documentSet.runCalculation(componentCalculation);
            Float result = componentCalculation.getResult();
            setComponentValue(result);
        }
    }

    @Override
    public boolean runCalculationIfMatch(String operand,
                                         ComponentCalculation calculation,
                                         DocumentInfoSet documentSet) {

        boolean operandMatched = false;

        if(operandMatched(operand)){

            calculateValueIfNeeded(documentSet);//calculate this component if necessary

            Float operandValue = getFloatValue();
            calculation.runOnOperand(operandValue);
        }

        return operandMatched;
    }

    private boolean operandMatched(String operand) {
        String componentName = getName();
        return componentName.equals(operand);
    }

    public void setComponentCalculation(ComponentCalculation componentCalculation) {
        this.componentCalculation = componentCalculation;
    }

}
