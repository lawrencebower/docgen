package org.lawrencebower.docgen.core.generator.overlay;

import com.lowagie.text.pdf.PdfReader;
import com.lowagie.text.pdf.PdfStamper;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.model.AbstractPDFGenerator;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.lawrencebower.docgen.core.generator.utils.StreamFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.io.InputStream;
import java.util.List;

public class OverlayPDFGenerator extends AbstractPDFGenerator<OverlayDocument> {

    private StreamFactory streamFactory;

    private OverlayDocument document;

    @Autowired(required = false)
    @Qualifier("pdfStreamFactory")
    public void setStreamFactory(StreamFactory streamFactory) {
        this.streamFactory = streamFactory;
    }

    @Override
    public PDFDocument generatePDF(OverlayDocument document) {

        this.document = document;

        checkRequiredValuesPresent();

        String sourcePDF = document.getSourcePDF();

        PdfReader pdfReader = getPDFReaderForSourcePDF(sourcePDF);

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

        List<OverlayComponent> components = document.getComponents();

        for (OverlayComponent component : components) {
            renderComponent(component, rendererInfo);
        }
    }

    private PdfStamper getPDFStamper(PdfReader pdfReader) {
        return pdfGenUtils.getPDFStamper(pdfReader, pdfOutStream);
    }

    private PdfReader getPDFReaderForSourcePDF(String sourcePDF) {

        InputStream inStream = streamFactory.getStreamFromFile(sourcePDF);

        return pdfGenUtils.getPDFReaderAndUnlockForSourcePDF(inStream);
    }

    @Override
    protected void checkRequiredValuesPresent() {
        pdfGenUtils.checkRequiredValuesPresent(document);
    }

    private void renderComponent(OverlayComponent component,
                                 OverlayComponentRendererInfo rendererInfo) {

        component.createAndRenderComponent(rendererInfo);
    }

}
