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

        for (DocComponent docComponent : docInfo.getComponents()) {
            renderComponent(docComponent, rendererInfo);
        }
    }

    @Override
    protected void checkRequiredValuesPresent() {
        pdfGenUtils.checkRequiredValuesPresent(docInfo);
    }

    private void renderComponent(DocComponent component, CustomComponentRendererInfo rendererInfo) {
        componentRenderer.renderComponent(component, rendererInfo);
    }

}
