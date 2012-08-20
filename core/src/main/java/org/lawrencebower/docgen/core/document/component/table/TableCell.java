package org.lawrencebower.docgen.core.document.component.table;

import org.lawrencebower.docgen.core.document.component.position.DocAlignment;

public class TableCell {

    private String value;
    DocAlignment verticalAlignment = DocAlignment.TOP;//default
    DocAlignment horizontalAlignment = DocAlignment.LEFT;//default

    public TableCell(String value) {
        this.value = value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public DocAlignment getVerticalAlignment() {
        return verticalAlignment;
    }

    public void setVerticalAlignment(DocAlignment verticalAlignment) {
        this.verticalAlignment = verticalAlignment;
    }

    public DocAlignment getHorizontalAlignment() {
        return horizontalAlignment;
    }

    public void setHorizontalAlignment(DocAlignment horizontalAlignment) {
        this.horizontalAlignment = horizontalAlignment;
    }
}
