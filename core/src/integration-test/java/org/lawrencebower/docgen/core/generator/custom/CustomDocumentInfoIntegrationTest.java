package org.lawrencebower.docgen.core.generator.custom;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponentFactory;
import org.lawrencebower.docgen.core.generator.custom.component.CustomTextComponent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;
import java.util.Arrays;

import static org.junit.Assert.assertTrue;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/integration-test-config.xml")
public class CustomDocumentInfoIntegrationTest extends AbstractIntegrationTest {

    @Autowired
    private CustomDocumentFactory documentFactory;
    @Autowired
    private CustomComponentFactory componentFactory;

    CustomDocument customDocumentInfo;

    @Before
    public void setup() {
        prepareDirs();
        customDocumentInfo = documentFactory.getCustomDocument("test name");
    }

    @Test
    public void testGeneratePDF_validObject_producesValidPDF() {

        String outFilePath = outputPackage + "custom_output.pdf";
        String expectedOutFilePath = inputPackage + "custom_expected_output.pdf";

        CustomComponent textComponent = generateSimpleTextComponent();

        customDocumentInfo.setComponents(Arrays.asList(textComponent));

        PDFDocument pdfDocument = customDocumentInfo.generatePDF();

        File outputFile = createOutputFilePathAndWriteFile(outFilePath, pdfDocument);

        File expectedFile = new File(expectedOutFilePath);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedFile, outputFile);

        assertTrue(fileSameAsExpected);
    }

    private CustomTextComponent generateSimpleTextComponent() {

        TextComponent textComponent = new TextComponent(HorizontalAlignment.LEFT, "39 York Street");

        return componentFactory.createCustomText(textComponent);
    }
}
