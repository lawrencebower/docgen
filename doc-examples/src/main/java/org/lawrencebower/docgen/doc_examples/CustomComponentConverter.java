package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.component.*;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.custom.component.*;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomComponentConverter {

    @Autowired
    private PDFGenUtils pdfUtils;

    public CustomComponent convertComponent(DocComponent component) {

/*        if (component.getComponentType() == DocComponentType.TEXT) {
            return new CustomTextComponent((TextComponent) component);
        } else if (component.getComponentType() == DocComponentType.TABLE) {
            return new CustomTableComponent((TableComponent) component, pdfUtils);
        } else if (component.getComponentType() == DocComponentType.NEWLINE) {
            return new CustomNewLineComponent((NewLineComponent) component);
        } else if (component.getComponentType() == DocComponentType.IMAGE) {
            return new CustomImageComponent((ImageComponent) component);
        } else if (component.getComponentType() == DocComponentType.TABLE_TEXT) {
            return new CustomTableTextComponent((TableTextComponent) component);
        } else if (component.getComponentType() == DocComponentType.LINE) {
            return new CustomLineComponent((LineComponent) component);
        } else if (component.getComponentType() == DocComponentType.CHECKBOX) {
            throw new UnsupportedOperationException("Check box not supported by Custom renderer");
        } else {
            throw new DocGenException("Doc component not recognized " + component.getClass());
        }*/

        return null;
    }

}
