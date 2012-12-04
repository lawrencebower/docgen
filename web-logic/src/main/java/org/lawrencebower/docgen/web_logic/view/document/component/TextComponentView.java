package org.lawrencebower.docgen.web_logic.view.document.component;

import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.business.injection.document.DocumentInjectionInfo;
import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
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
            setComponentValue(value);
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

        String trueString = Boolean.TRUE.toString();
        String falseString = Boolean.FALSE.toString();

        if (text.equals(trueString) || text.equals(falseString)) {
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

    @Override
    public void setDocumentInjectionFields(DocumentInjectionInfo injectionInfo) {

        String componentName = getName();

        if (isDocumentInjection()) {
            String value = injectionInfo.getFieldValueByName(componentName);
            setComponentValue(value);
        }

    }

    public boolean hasCalculation() {
        return componentCalculation != null;
    }

    @Override
    public void calculateValueIfNeeded(DocumentSet documentSet) {
        if (hasCalculation() && componentCalculation.isNotRun()) {
            componentCalculation.clearResult();
            documentSet.runCalculation(componentCalculation);
            String result = componentCalculation.getFormattedResult();
            setComponentValue(result);
        }
    }

    @Override
    public boolean runCalculationIfMatch(String operand,
                                         ComponentCalculation calculation,
                                         DocumentSet documentSet) {

        boolean operandMatched = false;

        if (operandMatched(operand)) {

            calculateValueIfNeeded(documentSet);//calculate this component if necessary

            Float operandValue = getFloatValue();
            calculation.runOnOperand(operandValue);
        }

        return operandMatched;
    }

    @Override
    public void copyFromDocument(DocumentView documentToCopy) {
        String thisName = getName();
        List<DocComponentView> matchingComponents = documentToCopy.getComponentViewsWithName(thisName);
        for (DocComponentView matchingComponent : matchingComponents) {
            String copyValue = matchingComponent.getStringValue();
            setComponentValue(copyValue);
        }
    }

    private boolean operandMatched(String operand) {
        String componentName = getName();
        return componentName.equals(operand);
    }

    public void setComponentCalculation(ComponentCalculation componentCalculation) {
        this.componentCalculation = componentCalculation;
    }

}
