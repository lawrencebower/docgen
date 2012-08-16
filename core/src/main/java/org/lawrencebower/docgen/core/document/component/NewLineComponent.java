package org.lawrencebower.docgen.core.document.component;

public class NewLineComponent extends DocComponent {

    public static final String NEWLINE_NAME = "newline";

    public NewLineComponent() {
        super(NEWLINE_NAME);
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.NEWLINE;
    }
}
