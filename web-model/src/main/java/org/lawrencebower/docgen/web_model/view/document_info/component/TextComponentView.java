package org.lawrencebower.docgen.web_model.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.List;

public class TextComponentView extends DocComponentView<TextComponent> {

    public TextComponentView(TextComponent docComponent) {
        super(docComponent);
        componentViewType = ComponentViewType.TEXT;
    }

    @Override
    public String getComponentValue() {
        return docComponent.getTextString();
    }

    @Override
    public void setComponentFromParamString(String value) {
       docComponent.setText(value);//no processing needed
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
