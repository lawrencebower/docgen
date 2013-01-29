package org.lawrencebower.docgen.core.generator.model.itext_component;

import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.lawrencebower.docgen.core.generator.model.itext_component.utils.ITextTableGenerator;
import org.springframework.beans.factory.annotation.Autowired;

public class ITextTableComponent extends AbstractITextComponent<PdfPTable,
        TableComponent<? extends TableRow, ? extends TableHeaderRow>> {

    @Autowired
    private ITextTableGenerator generator;

    @Override
    public PdfPTable createITextComponent() {
        return generator.generateTable(component);
    }
}
