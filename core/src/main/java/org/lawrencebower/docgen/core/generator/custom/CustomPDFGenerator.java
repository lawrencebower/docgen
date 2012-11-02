package org.lawrencebower.docgen.core.generator.custom;

import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.lawrencebower.docgen.core.generator.model.AbstractPDFGenerator;
import org.lawrencebower.docgen.core.document.PDFDocument;

public class CustomPDFGenerator extends AbstractPDFGenerator<CustomDocumentInfo> {

    private CustomDocumentInfo docInfo;

    @Override
    public PDFDocument generatePDF(CustomDocumentInfo docInfo) {

        this.docInfo = docInfo;

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

        for (CustomComponent docComponent : docInfo.getComponents()) {
            renderComponent(docComponent, rendererInfo);
        }
    }

    @Override
    protected void checkRequiredValuesPresent() {
        pdfGenUtils.checkRequiredValuesPresent(docInfo);
    }

    private void renderComponent(CustomComponent component,
                                 CustomComponentRendererInfo rendererInfo) {
        component.createAndRenderComponent(rendererInfo);
    }

}
