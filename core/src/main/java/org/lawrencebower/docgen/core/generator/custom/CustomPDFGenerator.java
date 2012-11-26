package org.lawrencebower.docgen.core.generator.custom;

import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.lawrencebower.docgen.core.generator.model.AbstractPDFGenerator;

public class CustomPDFGenerator extends AbstractPDFGenerator<CustomDocument> {

    private CustomDocument document;

    @Override
    public PDFDocument generatePDF(CustomDocument document) {

        this.document = document;

        checkRequiredValuesPresent();

        resetPDFOutputStream();

        CustomComponentRendererInfo rendererInfo = getRendererInfo();

        renderComponents(rendererInfo);

        closeDocument(rendererInfo);

        return getPDFFromPDFStream();
    }

    private CustomComponentRendererInfo getRendererInfo() {

        return new CustomComponentRendererInfo(pdfOutStream);
    }

    private void closeDocument(CustomComponentRendererInfo rendererInfo) {
        rendererInfo.closeDocument();
    }

    private void renderComponents(CustomComponentRendererInfo rendererInfo) {

        for (CustomComponent docComponent : document.getComponents()) {
            renderComponent(docComponent, rendererInfo);
        }
    }

    @Override
    protected void checkRequiredValuesPresent() {
        pdfGenUtils.checkRequiredValuesPresent(document);
    }

    private void renderComponent(CustomComponent component,
                                 CustomComponentRendererInfo rendererInfo) {
        component.createAndRenderComponent(rendererInfo);
    }

}
