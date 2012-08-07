package org.lawrencebower.docgen.core.document;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.core.generator.utils.ChecksumUtils;
import org.lawrencebower.docgen.core.generator.utils.DocGenFileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;
import java.util.Arrays;

import static org.junit.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class CustomDocumentInfoIntegrationTest {

    @Autowired
    CustomDocumentInfo customDocumentInfo;

    @Autowired
    DocGenFileUtils fileUtils;

    @Autowired
    ChecksumUtils checksumUtils;

    @Autowired
    @Qualifier("customPDFTestOutput")
    String testOutPDFFile;

//    @Autowired
//    @Qualifier("customPDFTestImageOutput")
//    String testOutImageFile;

    @Autowired
    @Qualifier("customPDFTestExample")
    String testExampleFile;

    @Test
    public void testGeneratePDF_validObject_producesValidPDF() {

        customDocumentInfo.setName("test name");

        DocComponent textComponent = generateSimpleTextComponent();

        customDocumentInfo.setComponents(Arrays.asList(textComponent));

        PDFDocument pdfDocument = customDocumentInfo.generatePDF();

        File outputFile = new File(testOutPDFFile);

        fileUtils.deleteFileIfAlreadyExists(outputFile);

        pdfDocument.writeToFile(outputFile);

        File expectedImageFile = new File(testExampleFile);
        File generatedImageFile = new File(testOutPDFFile);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedImageFile, generatedImageFile);

        assertTrue(fileSameAsExpected);
    }

    private DocComponent generateSimpleTextComponent() {

        DocCoordinates coordinates = new DocCoordinates(100, 675, 180, 81);
        DocPosition position = new DocPosition(DocAlignment.LEFT, coordinates);

        return new TextComponent("Address", position, "39 York Street");
    }
}
