package org.lawrencebower.docgen.core.document;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayPDFGenerator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;
import java.util.Arrays;

import static org.junit.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class OverlayDocumentInfoIntegrationTest extends AbstractIntegrationTest {

    @Autowired
    OverlayPDFGenerator pdfGenerator;

    @Before
    public void setup(){
        super.prepareDirs();
    }

    @Test
    public void testGeneratePDF_validObject_producesValidPDF() {

        String inputFilePath = inputPackage + "overlay_input.pdf";
        String expectedOutputFilePath = inputPackage + "overlay_expected_output.pdf";
        String outFilePath = outputPackage + "overlay_output.pdf";

        OverlayDocumentInfo overlayDocumentInfo = new OverlayDocumentInfo("test name", pdfGenerator);

        overlayDocumentInfo.setSourcePDF(inputFilePath);

        DocComponent textComponent = generateSimpleTextComponent();

        overlayDocumentInfo.setComponents(Arrays.asList(textComponent));

        PDFDocument pdfDocument = overlayDocumentInfo.generatePDF();

        File outputFile = createOutputFilePathAndWriteFile(outFilePath, pdfDocument);

        File expectedFile = new File(expectedOutputFilePath);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedFile, outputFile);

        assertTrue(fileSameAsExpected);
    }

    private DocComponent generateSimpleTextComponent() {

        DocCoordinates coordinates = new DocCoordinates(100, 675, 180, 81);

        TextComponent textComponent = new TextComponent(HorizontalAlignment.LEFT, "39 York Street");
        textComponent.setCoordinates(coordinates);

        return textComponent;
    }
}
