package org.lawrencebower.docgen.core.generator.custom.renderer;

import com.lowagie.text.Element;
import com.lowagie.text.pdf.PdfPTable;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.custom.CustomComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomTableRenderer
        implements DocComponentRenderer<TableComponent, CustomComponentRendererInfo, PdfPTable> {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Override
    public void createAndRenderComponent(TableComponent component, CustomComponentRendererInfo rendererInfo) {
        Element element = createComponent(component);
        renderComponent(rendererInfo, element);
    }

    @Override
    public PdfPTable createComponent(TableComponent component) {
        return pdfUtils.generateTable(component);
    }

    private void renderComponent(CustomComponentRendererInfo renderInfo,
                                 Element element) {
        renderInfo.addToDocument(element);
    }
}
