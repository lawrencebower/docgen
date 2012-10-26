package org.lawrencebower.docgen.core.generator.overlay.renderer;

import com.lowagie.text.BadElementException;
import com.lowagie.text.Image;
import com.lowagie.text.pdf.ColumnText;
import com.lowagie.text.pdf.PdfContentByte;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.DocComponentRenderer;
import org.lawrencebower.docgen.core.generator.overlay.OverlayComponentRendererInfo;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.IOException;

public class OverlayImageRenderer extends AbstractOverlayRenderer
        implements DocComponentRenderer<ImageComponent, OverlayComponentRendererInfo, Image> {

    @Autowired
    private PDFGenUtils pdfUtils;

    @Override
    public void createAndRenderComponent(ImageComponent component, OverlayComponentRendererInfo rendererInfo) {
        this.docComponent = component;
        Image image = createComponent(component);
        drawTable(rendererInfo.getCanvas(), image);
    }

    @Override
    public Image createComponent(ImageComponent component) {
        try {
            String imageFileLocation = component.getImageFileLocation();

            return Image.getInstance(imageFileLocation);
        } catch (BadElementException | IOException e) {
            throw new DocGenException(e);
        }
    }

    private void drawTable(PdfContentByte canvas, Image image) {

        DocCoordinates boxCoordinates = docComponent.getCoordinates();

        renderBorderIfSet(canvas, boxCoordinates);

        image.scaleToFit(boxCoordinates.getWidth(), boxCoordinates.getHeight());

        ColumnText column = createColumn(canvas,
                                         image,
                                         boxCoordinates);

        drawColumn(column);
    }

}
