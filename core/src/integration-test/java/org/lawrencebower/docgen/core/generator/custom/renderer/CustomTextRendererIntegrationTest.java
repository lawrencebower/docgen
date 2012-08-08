package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.CustomDocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
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
public class CustomTextRendererIntegrationTest extends AbstractIntegrationTest {

    @Autowired
    private CustomDocumentInfo docInfo;

    @Autowired
    private CustomPDFGenerator customGenerator;

    private String longText = "value sssssssss sss ssssssss ssssssss sssssssss ssssssssssss ssssssss ssssssss ssssssss ssssssss sssss sss sssssss sssss ss ssssssss ss sssssss ssssssss ssss ssssssss ssss sssssssss sssssssss";

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_validComponent_createsFile() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "text_renderer_output.pdf";

        DocComponent textComponent = new TextComponent("Name", "value");

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent);

    }

    @Test
    public void testRenderComponent_longtext_wrappsSuccessfully() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output2.pdf";
        String outFilePath = outputPackage + "text_renderer_output2.pdf";

        DocComponent textComponent = new TextComponent("Name", longText);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        textComponent);
    }

    @Test
    public void testRenderComponent_alignedText_allignedCorrectly() {

        String expectedOutputFilePath = inputPackage + "text_renderer_expected_output3.pdf";
        String outFilePath = outputPackage + "text_renderer_output3.pdf";

        DocPosition justifiedPosition = new DocPosition(DocAlignment.JUSTIFIED);
        DocComponent justifiedComponent = new TextComponent("Name",
                                                            justifiedPosition,
                                                            longText);

        DocPosition leftPosition = new DocPosition(DocAlignment.LEFT);
        DocComponent leftComponent = new TextComponent("Name",
                                                       leftPosition,
                                                       longText);

        DocPosition rightPosition = new DocPosition(DocAlignment.RIGHT);
        DocComponent rightComponent = new TextComponent("Name",
                                                        rightPosition,
                                                        longText);

        DocPosition centerPosition = new DocPosition(DocAlignment.CENTER);
        DocComponent centerComponent = new TextComponent("Name",
                                                         centerPosition,
                                                         longText);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        justifiedComponent,
                                        centerComponent,
                                        leftComponent,
                                        rightComponent);
    }

    private void createPDFAndCompareWithExpected(String expectedOutputFilePath,
                                                 String outFilePath,
                                                 DocComponent... components) {

        docInfo.setComponents(Arrays.asList(components));
        docInfo.setName("Doc name");

        PDFDocument pdfDocument = customGenerator.generatePDF(docInfo);

        File outputFile = createOutputFilePathAndWriteFile(outFilePath, pdfDocument);

        File expectedFile = new File(expectedOutputFilePath);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedFile, outputFile);

        assertTrue(fileSameAsExpected);
    }
}
