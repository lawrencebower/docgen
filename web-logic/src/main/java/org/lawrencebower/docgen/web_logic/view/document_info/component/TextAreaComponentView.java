package org.lawrencebower.docgen.web_logic.view.document_info.component;

/**
 * Same as TextComponentView, but is represented as a text area
 */
public class TextAreaComponentView extends TextComponentView {

    private TextAreaComponentView() {//force spring creation
        componentViewType = ComponentViewType.TEXT_AREA;
    }

    @Override
    public void setComponentValue(Boolean value) {
        super.setComponentValue(value);
    }
}
