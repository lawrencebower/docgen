package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.CustomDocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.generator.custom.CustomPDFGenerator;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.File;
import java.util.Arrays;

import static org.junit.Assert.assertTrue;

public abstract class AbstractCustomRendererTest extends AbstractIntegrationTest {

    @Autowired
    private CustomPDFGenerator customGenerator;

    protected AbstractCustomRendererTest() {
    }

    protected void createPDFAndCompareWithExpected(String expectedOutputFilePath,
                                                   String outFilePath,
                                                   DocComponent... components) {

        CustomDocumentInfo docInfo = new CustomDocumentInfo("Doc name", customGenerator);
        docInfo.setComponents(Arrays.asList(components));

        PDFDocument pdfDocument = docInfo.generatePDF();

        File outputFile = createOutputFilePathAndWriteFile(outFilePath, pdfDocument);

        File expectedFile = new File(expectedOutputFilePath);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedFile, outputFile);

        assertTrue(fileSameAsExpected);
    }
}
