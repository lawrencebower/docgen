package org.lawrencebower.docgen.web_model.view.document.component;

/**
 * Same as TextComponentView, but is represented as a text area
 */
public class TextAreaComponentView extends TextComponentView {

    private TextAreaComponentView() {//force spring creation
        componentViewType = ComponentViewType.TEXT_AREA;
    }

    @Override
    public boolean isTextArea() {
        return true;
    }

}
