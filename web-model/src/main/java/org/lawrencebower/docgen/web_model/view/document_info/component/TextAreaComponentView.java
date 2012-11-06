package org.lawrencebower.docgen.web_model.view.document_info.component;

import org.lawrencebower.docgen.core.document.component.TextComponent;

/**
 * Same as TextComponentView, but is represented as a text area
 */
public class TextAreaComponentView extends TextComponentView {

    public TextAreaComponentView(TextComponent docComponent) {
        super(docComponent);
        this.componentViewType = ComponentViewType.TEXT_AREA;
    }

}
