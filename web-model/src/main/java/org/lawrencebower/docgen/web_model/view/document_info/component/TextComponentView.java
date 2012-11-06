package org.lawrencebower.docgen.web_model.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.web_model.view.document_info.DocComponentView;

public class TextComponentView extends DocComponentView<TextComponent> {

    public TextComponentView(TextComponent docComponent) {
        super(docComponent);
        this.componentViewType = ComponentViewType.TEXT;
    }

    @Override
    public String getComponentValue() {
        return docComponent.getTextString();
    }

    @Override
    public void setComponentFromParamString(String value) {
       docComponent.setText(value);//no processing needed
    }

}
