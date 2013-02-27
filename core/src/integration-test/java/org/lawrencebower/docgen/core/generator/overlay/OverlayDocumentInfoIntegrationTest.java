package org.lawrencebower.docgen.core.generator.overlay;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponentFactory;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayTextComponent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/integration-test-config.xml")
public class OverlayDocumentInfoIntegrationTest extends AbstractIntegrationTest {

    @Autowired
    private OverlayDocumentFactory documentFactory;
    @Autowired
    private OverlayComponentFactory componentFactory;

    @Before
    public void setup(){
        prepareDirs();
    }

    @Test
    public void testGeneratePDF_validObject_producesValidPDF() {

        /**
         * this is read from the classpath
         */
        Resource inputFilePath = new ClassPathResource("/org/lawrencebower/docgen/core/generator/overlay/overlay_input.pdf");

        /**
         * these are read from file system
         */
        String expectedOutputFilePath = inputPackage + "overlay_expected_output.pdf";
        String outFilePath = outputPackage + "overlay_output.pdf";

        OverlayDocument overlayDocumentInfo = documentFactory.getOverlayDocument("test name");

        overlayDocumentInfo.setSourcePDF(inputFilePath);

        OverlayComponent textComponent = generateSimpleTextComponent();

        List<OverlayComponent> components = Arrays.asList(textComponent);

        overlayDocumentInfo.setComponents(components);

        PDFDocument pdfDocument = overlayDocumentInfo.generatePDF();

        File outputFile = createOutputFilePathAndWriteFile(outFilePath, pdfDocument);

        File expectedFile = classPathResourceToFile(expectedOutputFilePath);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedFile, outputFile);

        assertTrue(fileSameAsExpected);
    }

    private OverlayTextComponent generateSimpleTextComponent() {

        DocCoordinates coordinates = new DocCoordinates(100, 675, 180, 81);

        TextComponent textComponent = new TextComponent(HorizontalAlignment.LEFT, "39 York Street");
        textComponent.setCoordinates(coordinates);

        return componentFactory.createOverlayText(textComponent);
    }
}
