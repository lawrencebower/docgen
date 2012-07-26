package org.lawrencebower.docgen.core.generator.custom;

import org.lawrencebower.docgen.core.document.CustomDocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.generator.custom.renderer.CustomComponentRenderer;
import org.lawrencebower.docgen.core.generator.model.AbstractPDFGenerator;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.springframework.beans.factory.annotation.Autowired;

public class CustomPDFGenerator extends AbstractPDFGenerator<CustomDocumentInfo> {

    private CustomDocumentInfo docInfo;

    private CustomComponentRenderer componentRenderer;

    @Autowired
    public void setComponentRenderer(CustomComponentRenderer componentRenderer) {
        this.componentRenderer = componentRenderer;
    }

    @Override
    public PDFDocument generatePDF(CustomDocumentInfo docInfo) {

        this.docInfo = docInfo;

        checkRequiredValuesPresent();

        //   comment
        resetPDFOutputStream();

        rendererInfo rendererInfo = getRendererInfo();

        preparePDFWriter(rendererInfo);

        renderComponents(rendererInfo);

        closeDocument(rendererInfo);

        return getPDFFromPDFStream();
    }

    private rendererInfo getRendererInfo() {
        return new rendererInfo();
    }

    private void closeDocument(rendererInfo rendererInfo) {
        rendererInfo.closeDocument();
    }

    private void renderComponents(rendererInfo rendererInfo) {

        for (DocComponent docComponent : docInfo.getComponents()) {
            renderComponent(docComponent, rendererInfo);
        }
    }

    private void preparePDFWriter(rendererInfo rendererInfo) {
        rendererInfo.preparePDFWriter(pdfOutStream);
    }

    @Override
    protected void checkRequiredValuesPresent() {
        pdfGenUtils.checkRequiredValuesPresent(docInfo);
    }

    public void renderComponent(DocComponent component, rendererInfo rendererInfo) {
        componentRenderer.renderComponent(component, rendererInfo);
    }

}
