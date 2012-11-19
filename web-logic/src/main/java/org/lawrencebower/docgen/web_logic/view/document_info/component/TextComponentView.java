package org.lawrencebower.docgen.web_logic.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.web_logic.business.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_logic.view.document_info.DocComponentView;
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
    public void checkAndSetValueFromParamString(String componentName, String value) {
        String thisComponentName = getName();
        if (componentName.equals(thisComponentName)) {
            docComponent.setText(value);
        }
    }

    public String getComponentText() {
        return docComponent.getTextString();
    }

    @Override
    public void injectProducts(List<ProductView> products) {
        //not implemented - just exit quietly
    }

    public boolean hasComponentCalculations() {
        return componentCalculation != null;
    }

    @Override
    public void calculateValue(List<DocComponentView> allComponents) {
        if (hasComponentCalculations()) {
            componentCalculator.calculateComponentValue(this, allComponents);
        }
    }

    public void setComponentCalculation(ComponentCalculation componentCalculation) {
        this.componentCalculation = componentCalculation;
    }

}
