package org.lawrencebower.docgen.core.generator.overlay;

import com.lowagie.text.pdf.PdfContentByte;
import com.lowagie.text.pdf.PdfStamper;
import org.lawrencebower.docgen.core.document.DocComponentRendererInfo;

public class OverlayComponentRendererInfo implements DocComponentRendererInfo {

    private PdfStamper pdfStamper;

    public OverlayComponentRendererInfo(PdfStamper pdfStamper) {
        this.pdfStamper = pdfStamper;
    }

    public PdfContentByte getCanvas() {
        return pdfStamper.getOverContent(1);
    }
}
