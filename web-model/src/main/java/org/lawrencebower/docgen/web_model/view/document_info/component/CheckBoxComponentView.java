package org.lawrencebower.docgen.web_model.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.List;

public class CheckBoxComponentView<T extends DocComponent> extends DocComponentView<CheckBoxComponent> {

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
        }else{
            String messageTemplate = "Can not map String '%s' to checkbox selection";
            String message = String.format(messageTemplate, value);
            throw new DocGenException(message);
        }
    }

    @Override
    public void setComponent(CheckBoxComponent docComponent) {
        super.setComponent(docComponent);
    }

    public String getComponentValue() {
        if (docComponent.isSelected()) {
            return SELECTED_TEXT;
        }
        return UNSELECTED_TEXT;
    }

    @Override
    public void checkAndSetValueFromParamString(String componentName, String value) {
        throw new NotImplementedException();//todo implement this
    }

    @Override
    public boolean allowsProductInjection() {
        return false;
    }

    @Override
    public void injectProducts(List<ProductView> products) {
        throw new NotImplementedException();
    }

}
