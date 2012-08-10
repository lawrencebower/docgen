package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomTableRenderer implements DocComponentRenderer<TableComponent, CustomComponentRendererInfo> {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Override
    public void renderComponent(TableComponent component, CustomComponentRendererInfo rendererInfo) {

        PdfPTable table = pdfUtils.generateTable(component);

        rendererInfo.addToDocument(table);
    }
}
