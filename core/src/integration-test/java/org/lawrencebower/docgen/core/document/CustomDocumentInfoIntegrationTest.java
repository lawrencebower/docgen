package org.lawrencebower.docgen.core.document;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.generator.custom.CustomPDFGenerator;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;
import java.util.Arrays;

import static org.junit.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class CustomDocumentInfoIntegrationTest extends AbstractIntegrationTest {

    @Autowired
    private CustomPDFGenerator pdfGenerator;

    CustomDocumentInfo customDocumentInfo;

    @Before
    public void setup(){
        super.prepareDirs();
        customDocumentInfo = new CustomDocumentInfo("test name", pdfGenerator);
    }

    @Test
    public void testGeneratePDF_validObject_producesValidPDF() {

        String outFilePath = outputPackage + "custom_output.pdf";
        String expectedOutFilePath = inputPackage + "custom_expected_output.pdf";

        DocComponent textComponent = generateSimpleTextComponent();

        customDocumentInfo.setComponents(Arrays.asList(textComponent));

        PDFDocument pdfDocument = customDocumentInfo.generatePDF();

        File outputFile = createOutputFilePathAndWriteFile(outFilePath, pdfDocument);

        File expectedFile = new File(expectedOutFilePath);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedFile, outputFile);

        assertTrue(fileSameAsExpected);
    }

    private DocComponent generateSimpleTextComponent() {

        return new TextComponent(HorizontalAlignment.LEFT, "39 York Street");
    }
}
