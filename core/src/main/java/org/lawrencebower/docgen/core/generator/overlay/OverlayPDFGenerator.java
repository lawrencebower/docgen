package org.lawrencebower.docgen.core.generator.overlay;

import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.lawrencebower.docgen.core.document.OverlayDocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.AbstractPDFGenerator;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.renderer.OverlayComponentRenderer;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;
import java.util.List;

public class OverlayPDFGenerator extends AbstractPDFGenerator<OverlayDocumentInfo> {

    private OverlayDocumentInfo docInfo;

    @Autowired
    private OverlayComponentRenderer componentRenderer;

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
        try {
            pdfStamper.close();
        } catch (DocumentException | IOException e) {
            throw new DocGenException(e);
        }
    }

    private void drawComponentsWithStamper(PdfStamper pdfStamper) {

        OverlayComponentRendererInfo rendererInfo = new OverlayComponentRendererInfo(pdfStamper);

        List<? extends DocComponent> components = docInfo.getComponents();

        for (DocComponent component : components) {
            renderComponent(component, rendererInfo);
        }
    }

    private PdfStamper getPDFStamper(PdfReader pdfReader) {
        try {
            return new PdfStamper(pdfReader, pdfOutStream);
        } catch (DocumentException | IOException e) {
            throw new DocGenException(e);
        }
    }

    private PdfReader getPDFReaderForSourcePDF(String sourcePDF) {
        try {
            return new PdfReader(sourcePDF);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    @Override
    protected void checkRequiredValuesPresent() {
        pdfGenUtils.checkRequiredValuesPresent(docInfo);
    }

    public void renderComponent(DocComponent component,
                                OverlayComponentRendererInfo rendererInfo) {

         componentRenderer.renderComponent(component, rendererInfo);
    }

}
