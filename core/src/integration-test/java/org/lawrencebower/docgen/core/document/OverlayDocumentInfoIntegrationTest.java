package org.lawrencebower.docgen.core.document;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;
import java.util.Arrays;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class OverlayDocumentInfoIntegrationTest {

    @Autowired
    OverlayDocumentInfo overlayDocumentInfo;

    @Autowired
    @Qualifier("overlayPDFTestExample")
    String exampleSourcePDF;

    @Autowired
    @Qualifier("overlayPDFTestOutput")
    String testOutFile;

    @Test
    public void testGeneratePDF_validObject_producesValidPDF() {

        overlayDocumentInfo.setName("test name");

        overlayDocumentInfo.setSourcePDF(exampleSourcePDF);

        DocComponent textComponent = generateSimpleTextComponent();

        overlayDocumentInfo.setComponents(Arrays.asList(textComponent));

        PDFDocument pdfDocument = overlayDocumentInfo.generatePDF();

        pdfDocument.writeToFile(new File(testOutFile));
    }

    private DocComponent generateSimpleTextComponent() {

        DocCoordinates coordinates = new DocCoordinates(100, 675, 180, 81);
        DocPosition position = new DocPosition(DocAlignment.LEFT, coordinates);

        return new TextComponent("Address", position, "39 York Street");
    }
}
