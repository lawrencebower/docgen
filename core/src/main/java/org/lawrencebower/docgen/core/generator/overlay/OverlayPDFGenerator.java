package org.lawrencebower.docgen.core.generator.overlay;

import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.lawrencebower.docgen.core.generator.model.AbstractPDFGenerator;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;

import java.util.List;

public class OverlayPDFGenerator extends AbstractPDFGenerator<OverlayDocumentInfo> {

    private OverlayDocumentInfo docInfo;

    @Override
    public PDFDocument generatePDF(OverlayDocumentInfo docInfo) {

        this.docInfo = docInfo;

        checkRequiredValuesPresent();

        PdfReader pdfReader = getPDFReaderForSourcePDF(docInfo.getSourcePDF());

        resetPDFOutputStream();

        PdfStamper pdfStamper = getPDFStamper(pdfReader);

        drawComponentsWithStamper(pdfStamper);

        closeStamper(pdfStamper);

        return getPDFFromPDFStream();
    }

    private void closeStamper(PdfStamper pdfStamper) {
        pdfGenUtils.closePDFStamper(pdfStamper);
    }

    private void drawComponentsWithStamper(PdfStamper pdfStamper) {

        OverlayComponentRendererInfo rendererInfo = new OverlayComponentRendererInfo(pdfStamper);

        List<OverlayComponent> components = docInfo.getComponents();

        for (OverlayComponent component : components) {
            renderComponent(component, rendererInfo);
        }
    }

    private PdfStamper getPDFStamper(PdfReader pdfReader) {
        return pdfGenUtils.getPDFStamper(pdfReader, pdfOutStream);
    }

    private PdfReader getPDFReaderForSourcePDF(String sourcePDF) {
        return pdfGenUtils.getPDFReaderAndUnlockForSourcePDF(sourcePDF);
    }

    @Override
    protected void checkRequiredValuesPresent() {
        pdfGenUtils.checkRequiredValuesPresent(docInfo);
    }

    private void renderComponent(OverlayComponent component,
                                 OverlayComponentRendererInfo rendererInfo) {

        component.createAndRenderComponent(rendererInfo);
    }

}
