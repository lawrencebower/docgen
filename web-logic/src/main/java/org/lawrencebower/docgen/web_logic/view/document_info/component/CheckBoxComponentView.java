package org.lawrencebower.docgen.web_logic.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.view.document_info.DocumentInfoSet;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.List;

public class CheckBoxComponentView extends DocComponentView<CheckBoxComponent> {

    protected static final String SELECTED_TEXT = "X";
    protected static final String UNSELECTED_TEXT = "";

    public CheckBoxComponentView() {
        componentViewType = ComponentViewType.CHECK_BOX;
    }

    @Override
    public void setComponentValue(Boolean selected) {
        docComponent.setSelected(selected);
    }

    @Override
    public void setComponentValue(String value) {

        String trueString = Boolean.TRUE.toString();
        String falseString = Boolean.FALSE.toString();

        if (value.equals(trueString)) {
            docComponent.setSelected(true);
        } else if (value.equals(falseString)) {
            docComponent.setSelected(false);
        } else {
            String messageTemplate = "Can not map String '%s' to checkbox selection";
            String message = String.format(messageTemplate, value);
            throw new DocGenException(message);
        }
    }

    @Override
    public void setComponentValue(Float value) {
        throw new DocGenException("Can not set a CheckBox value as Float");
    }

    @Override
    public void setComponent(CheckBoxComponent docComponent) {
        super.setComponent(docComponent);
    }

    public String getStringValue() {
        if (docComponent.isSelected()) {
            return SELECTED_TEXT;
        }
        return UNSELECTED_TEXT;
    }

    @Override
    public Float getFloatValue() {
        throw new DocGenException("Can not get a CheckBox value as Float");
    }

    @Override
    public Boolean getBooleanValue() {
        return docComponent.isSelected();
    }

    @Override
    public void checkAndSetValueFromParamString(String componentName, String value) {
        throw new NotImplementedException();//todo implement this
    }

    @Override
    public void injectProducts(List<ProductView> products) {
        //not implemented - just exit quietly
    }

    @Override
    public boolean hasCalculation() {
        return false;//not supported
    }

    @Override
    public void calculateValueIfNeeded(DocumentInfoSet documentSet) {
        throw new DocGenException("CheckBox does not support calculated values");
    }

    @Override
    public boolean runCalculationIfMatch(String operand,
                                         ComponentCalculation calculation,
                                         DocumentInfoSet documentSet) {
        return false;//not supported
    }

}
