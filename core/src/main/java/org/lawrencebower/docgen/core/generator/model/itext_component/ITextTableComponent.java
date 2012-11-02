package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.Element;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class ITextTableComponent extends AbstractITextComponent<PdfPTable, TableComponent> {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Override
    public PdfPTable createITextComponent() {
        return pdfUtils.generateTable(component);
    }
}
