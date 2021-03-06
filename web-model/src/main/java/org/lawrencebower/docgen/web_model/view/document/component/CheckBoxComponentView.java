package org.lawrencebower.docgen.web_model.view.document.component;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.component_calculation.ComponentCalculation;
import org.lawrencebower.docgen.web_model.view.document.DocumentInjectionInfo;
import org.lawrencebower.docgen.web_model.view.document.DocumentSet;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;

import java.util.List;

public class CheckBoxComponentView extends DocComponentViewImpl<CheckBoxComponent> {

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
    public void setComponent(CheckBoxComponent docComponent) {
        super.setComponent(docComponent);
    }

    public String getStringValue() {

        String returnValue = UNSELECTED_TEXT;

        if (docComponent.isSelected()) {
            returnValue = SELECTED_TEXT;
        }

        return returnValue;
    }

    @Override
    public Boolean getBooleanValue() {
        return docComponent.isSelected();
    }

    @Override
    public void injectProducts(List<ProductView> products) {
        //not implemented - just exit quietly
    }

    @Override
    public void setDocumentInjectionFields(DocumentInjectionInfo injectionInfo) {
        //not implemented - just exit quietly
    }

    @Override
    public boolean hasCalculation() {
        return false;//not supported
    }

    @Override
    public void setComponentCalculation(ComponentCalculation calculation) {
        throw new DocGenException("ComponentCalculation not supported by CheckBoxComponentView");
    }

    @Override
    public void calculateValueIfNeeded(DocumentSet documentSet) {
        //not implemented - just exit quietly
    }

    @Override
    public boolean runCalculationIfMatch(String operand,
                                         ComponentCalculation calculation,
                                         DocumentSet documentSet) {
        return false;//not supported
    }

    @Override
    public void copyFromDocument(DocumentView documentToCopy) {
        String thisName = getName();
        List<DocComponentView> matchingComponents = documentToCopy.getComponentViewsWithName(thisName);
        for (DocComponentView matchingComponent : matchingComponents) {
            Boolean copyValue = matchingComponent.getBooleanValue();
            this.setComponentValue(copyValue);
        }
    }

}
