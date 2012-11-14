package org.lawrencebower.docgen.web_model.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.List;

public class CheckBoxComponentView<T extends DocComponent> extends DocComponentView<CheckBoxComponent> {

    protected static final String SELECTED_TEXT = "X";
    protected static final String UNSELECTED_TEXT = "";

    public CheckBoxComponentView(CheckBoxComponent docComponent) {
        super(docComponent);
        this.componentViewType = ComponentViewType.CHECK_BOX;
    }

    @Override
    public String getComponentValue() {
        if(docComponent.isSelected()){
            return SELECTED_TEXT;
        }
        return UNSELECTED_TEXT;
    }

    @Override
    public void setComponentFromParamString(String value) {
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
