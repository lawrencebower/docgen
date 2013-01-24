package org.lawrencebower.docgen.core.document.component;

public class NewLineComponent extends AbstractDocComponent {

    public static final String NEWLINE_NAME = "newline";

    public NewLineComponent() {
        super();
        setName(NEWLINE_NAME);
    }

    @Override
    public DocComponentType getComponentType() {
        return DocComponentType.NEWLINE;
    }
}
