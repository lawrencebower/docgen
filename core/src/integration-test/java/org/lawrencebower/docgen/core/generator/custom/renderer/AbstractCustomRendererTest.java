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

public abstract class AbstractCustomRendererTest extends AbstractIntegrationTest{

    @Autowired
    private CustomDocumentInfo docInfo;

    @Autowired
    private CustomPDFGenerator customGenerator;

    protected void createPDFAndCompareWithExpected(String expectedOutputFilePath,
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
